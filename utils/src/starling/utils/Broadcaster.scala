package starling.utils

import swing.event.Event
import scala.collection.JavaConversions._
import ImplicitConversions._
import starling.manager.{QueuedReceiver, Receiver, Broadcaster}

class ReceiversBroadcaster extends Broadcaster with Log {
  val receivers = new java.util.concurrent.ConcurrentHashMap[AnyRef,Receiver]()

  def addReceiver(ref:AnyRef, receiver:Receiver) {
    receivers.put(ref, new QueuedReceiver(receiver))
  }

  def removeReceiver(ref:AnyRef) {
    receivers.remove(ref)
  }

  def broadcast(event: Event) {
    receivers.values().foreach(_.event(event))
  }
}

class CompositeBroadcaster(broadcasters: (Boolean, Broadcaster)*) extends Broadcaster {
  private val enabledBroadcasters = broadcasters.filter(_._1).map(_._2)

  def broadcast(event: Event) = enabledBroadcasters.map(_.broadcast(event))
}

abstract class TypedBroadcaster[T](implicit manifest: Manifest[T]) extends Broadcaster {
  override def broadcast(event: Event) = manifest.safeCast(event).foreach(typedBroadcast)

  def typedBroadcast(t: T)
}