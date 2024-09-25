package kvstore

import akka.actor.{Actor, ActorRef, InvalidMessageException, Props}

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import context.dispatcher

  // map from seq to requests
  var futureResponses = Map.empty[Long, (ActorRef, Replicated)]

  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pendingRequests = Vector.empty[Snapshot]
  
  private var _seqCounter = 0L
  def nextSeq(): Long = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  override def preStart(): Unit = {
    context.system.scheduler.scheduleAtFixedRate(100.milliseconds, 100.milliseconds)(() => batchAndSend())
  }
  
  def receive: Receive = {
    case Replicate(key, value, id) =>
      if (!futureResponses.exists { case (_, (_, Replicated(_, i))) => i == id }) {
        val seq = nextSeq()
        pendingRequests = pendingRequests.appended(Snapshot(key, value, seq))
        futureResponses = futureResponses.updated(seq, (sender, Replicated(key, id)))
      }
    case SnapshotAck(_, seq) =>
      futureResponses get seq match {
        case None => ()
        case Some((s, msg)) =>
          pendingRequests = pendingRequests.filter(_.seq != seq)
          s ! msg
      }
    case msg => throw InvalidMessageException("replicator receive: " + msg)
  }

  private def batchAndSend(): Unit = {
    val batchedPending = pendingRequests
      .groupBy { case Snapshot(key, _, _) => key }
      .map(_._2.last)
      .toVector
    pendingRequests = batchedPending
    pendingRequests.foreach { replica ! _ }
  }
}
