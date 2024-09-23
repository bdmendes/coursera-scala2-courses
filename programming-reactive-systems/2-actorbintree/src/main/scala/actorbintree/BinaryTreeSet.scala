/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._

import scala.collection.immutable.Queue

object BinaryTreeSet {

  sealed trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection */
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional (used to stash incoming operations during garbage collection)
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case msg:Operation => root ! msg
    case GC =>
      val newRoot = createRoot
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
    case msg => throw InvalidMessageException("In set normal: " + msg.toString)
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case CopyFinished =>
      context.stop(root)
      root = newRoot
      context.become(normal)
      // Note: It is important to send the message to the root
      // instead of self. Sending to self, the message will get
      // processed AFTER the messages already in the internal
      // actor queue and mess up the ordering guarantees.
      pendingQueue.foreach {root ! _}
      pendingQueue = Queue.empty
    case GC =>
      ()
    case msg:Operation =>
      pendingQueue = pendingQueue.enqueue(msg)
    case msg => throw InvalidMessageException("Set GC: " + msg.toString)
  }
}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  /**
   * Acknowledges that a copy has been completed. This message should be sent
   * from a node to its parent, when this node and all its children nodes have
   * finished being copied.
   */
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  private def childContaining(e: Int) = {
    if (elem > e) Right else Left
  }

  // optional
  def receive: Receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  private val normal: Receive = {
    // Note that there is no need to forward back crud children responses,
    // since they respond to the original sender (passed in the messages).
    case Contains(req, id, e) =>
      if (elem == e && !removed) {
        req ! ContainsResult(id, result = true)
      } else {
        subtrees get childContaining(e) match {
          case Some(c) => c ! Contains(req, id, e)
          case None =>
            req ! ContainsResult(id, result = false)
        }
      }
    case Insert(req, id, e) =>
      if (e != elem) {
        subtrees get childContaining(e) match {
          case Some(node) =>
            node ! Insert(req, id, e)
          case None =>
            subtrees = subtrees + (childContaining(e) -> context.actorOf(props(e, initiallyRemoved = false)))
            req ! OperationFinished(id)
        }
      } else {
        removed = false
        req ! OperationFinished(id)
      }
    case Remove(req, id, e) =>
      if (e == elem && !removed) {
        removed = true
        req ! OperationFinished(id)
      } else {
        val child = childContaining(e)
        subtrees get child match {
          case Some(node) => node ! Remove(req, id, e)
          case None => req ! OperationFinished(id)
        }
      }
    case CopyTo(treeNode) =>
      if (removed && subtrees.isEmpty) {
        context.parent ! CopyFinished
      } else {
        context.become(copying(subtrees.values.toSet, insertConfirmed = removed))
        if (!removed) {
          treeNode ! Insert(self, elem, elem)
        }
        subtrees.foreach { case (_, tree) => tree ! CopyTo(treeNode) }
      }
    case msg => throw InvalidMessageException("Node normal: " + msg.toString)
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(_) =>
      if (expected.isEmpty) {
        context.parent ! CopyFinished
        context.stop(self)
      } else {
        context.become(copying(expected, insertConfirmed = true))
      }
    case CopyFinished =>
      val nextExpected = expected.filterNot(_ == sender)
      if (nextExpected.nonEmpty || !insertConfirmed) {
        context.become(copying(nextExpected, insertConfirmed))
      } else {
        context.parent ! CopyFinished
        context.stop(self)
      }
    case msg => throw InvalidMessageException("Node copying: " + msg.toString)
  }
}
