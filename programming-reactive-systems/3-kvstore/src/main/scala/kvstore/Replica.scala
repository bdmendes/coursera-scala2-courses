package kvstore

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{Actor, ActorRef, InvalidMessageException, OneForOneStrategy, Props, SupervisorStrategy}
import kvstore.Arbiter._

import java.time.Instant.now
import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDateTime, Period}
import scala.concurrent.duration._

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  override def preStart(): Unit = {
    arbiter ! Join
  }
  
  var kv = Map.empty[String, String]

  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]

  // the current set of replicators, along with expected sequence numbers.
  private var replicators: Map[ActorRef, Long] = Map.empty[ActorRef, Long] withDefaultValue 0

  // Track persist and replicate requests.
  private var persistMailbox = Map.empty[Long, (ActorRef, Persist, Instant)]
  private var replicateMailbox = Map.empty[Long, (ActorRef, Set[ActorRef], Replicate, Instant)]

  override val supervisorStrategy: SupervisorStrategy = OneForOneStrategy() {
    case _:PersistenceException => Restart
  }

  val persistor = context.watch(context.actorOf(persistenceProps))

  def receive: Receive = {
    case JoinedPrimary   =>
      context.become(leader)
      context.system.scheduler.scheduleAtFixedRate(100.milliseconds, 100.milliseconds)(() => sendRequests(true))
    case JoinedSecondary =>
      context.become(replica)
      context.system.scheduler.scheduleAtFixedRate(100.milliseconds, 100.milliseconds)(() => sendRequests(false))
    case _ =>
      throw new RuntimeException("Should not receive any messages with no role")
  }

  val leader: Receive = {
    case Replicas(set: Set[ActorRef]) =>
      var id = Long.MinValue
      val newReplicas = set.filter {r => !secondaries.keys.toSet.contains(r) && r != self }
      newReplicas.foreach { s =>
          val replicator = context.actorOf(Props(classOf[Replicator], s))
          secondaries = secondaries.updated(s, replicator)
          kv foreach { case (k, v) =>
            replicator ! Replicate(k, Some(v), id)
            id = id + 1
        }
      }

      val removedReplicas = secondaries.filter { case (r, _) => !set.contains(r) }
      removedReplicas.foreach { r =>
        replicateMailbox = replicateMailbox.map { case (id, (s, replicas, msg, time)) =>
          (id, (s, replicas.filter(_ != r._2), msg, time))
        }
        val danglingReplicators = replicateMailbox.filter { case (_, (_, replicas, _, _)) =>
          replicas.isEmpty
        }
        danglingReplicators.foreach { case (seq, (s, _, _, _)) =>
          replicateMailbox = replicateMailbox.removed(seq)
          s ! OperationAck(seq)
        }
        context.stop(r._2)
        secondaries = secondaries.removed(r._1)
      }
    case msg: Insert => insert(msg)
    case msg: Remove => remove(msg)
    case msg: Get => get(msg)
    case Persisted(_, id) =>
      persistMailbox get id match {
        case None => ()
        case Some((s, _, _)) =>
          persistMailbox = persistMailbox.removed(id)
          if (replicateMailbox.get(id).forall(_._2.isEmpty)) {
            s ! OperationAck(id)
          }
      }
    case Replicated(_, id) =>
      replicateMailbox get id match {
        case None => ()
        case Some((s, destinations, msg, time)) =>
          val newDestinations = destinations.filterNot(_ == sender)
          if (newDestinations.isEmpty) {
            replicateMailbox = replicateMailbox.removed(id)
            if (!persistMailbox.contains(id)) {
              s ! OperationAck(id)
            }
          } else {
            replicateMailbox = replicateMailbox.updated(id, (s, newDestinations, msg, time))
          }
      }
    case msg =>
      throw InvalidMessageException("leader handler: " + msg)
  }

  val replica: Receive = {
    case msg: Get =>
      get(msg)
    case Snapshot(key, value, seq) if replicators(sender) == seq =>
      val msg = Persist(key, value, seq)
      persistMailbox = persistMailbox.updated(seq, (sender, msg, now()))
      replicators = replicators.updated(sender, replicators(sender) + 1)
      persist(msg)
    case Snapshot(key, _, seq) if replicators(sender) > seq =>
      sender ! SnapshotAck(key, seq)
    case Snapshot(_, _, _) =>
      ()
    case Persisted(key, seq) =>
      persistMailbox get seq match {
        case None => throw new RuntimeException("should be in mailbox")
        case Some((sender, _, _)) =>
          persistMailbox = persistMailbox.removed(seq)
          sender ! SnapshotAck(key, seq)
      }
    case _:Replicas => ()
    case msg =>
      throw InvalidMessageException("replica handler: " + msg)
  }

  private def sendRequests(timeout: Boolean): Unit = {
    val pMailBox = persistMailbox
    pMailBox.foreach {
      case (seq, (s, msg, time)) =>
        if (timeout && time.until(now(), ChronoUnit.SECONDS) >= 1) {
          s ! OperationFailed(msg.id)
          persistMailbox = persistMailbox.removed(seq)
          replicateMailbox = replicateMailbox.removed(seq)
        } else {
          persistor ! msg
        }
    }

    val rMailBox = replicateMailbox
    rMailBox.foreach {
      case (id, (s, replicators, msg, time)) =>
        if (timeout && time.until(now(), ChronoUnit.SECONDS) >= 1) {
          s ! OperationFailed(msg.id)
          replicateMailbox = replicateMailbox.removed(id)
          persistMailbox = persistMailbox.removed(id)
        } else {
          replicators foreach { _ ! msg }
        }
    }
  }

  def persist(msg: Persist) = {
    msg match {
      case Persist(key, Some(value), _) =>
        kv = kv.updated(key, value)
      case Persist(key, None, _) =>
        kv = kv.removed(key)
    }
  }

  def insert(msg: Insert) = {
    kv = kv.updated(msg.key, msg.value)

    val persistMsg = Persist(msg.key, Some(msg.value), msg.id)
    persistor ! persistMsg
    persistMailbox = persistMailbox.updated(msg.id, (sender, persistMsg, now()))

    val replicateMsg = Replicate(msg.key, Some(msg.value), msg.id)
    secondaries.values foreach {
      _ ! replicateMsg
    }
    if (secondaries.nonEmpty) {
      replicateMailbox = replicateMailbox.updated(msg.id, (sender, secondaries.values.toSet, replicateMsg, now()))
    }
  }

  def remove(msg: Remove) = {
    kv = kv.removed(msg.key)

    val persistMsg = Persist(msg.key, None, msg.id)
    persistor ! persistMsg
    persistMailbox = persistMailbox.updated(msg.id, (sender, persistMsg, now()))

    val replicateMsg = Replicate(msg.key, None, msg.id)
    secondaries.values foreach {
      _ ! replicateMsg
    }
    if (secondaries.nonEmpty) {
      replicateMailbox = replicateMailbox.updated(msg.id, (sender, secondaries.values.toSet, replicateMsg, now()))
    }
  }

  def get(msg: Get) = sender ! GetResult(msg.key, kv.get(msg.key), msg.id)
}
