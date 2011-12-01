package starling.utils

import java.util.concurrent.Executors
import swing.event.Event
import scala.collection.JavaConversions
import ImplicitConversions._
import starling.manager.{Receiver, Broadcaster}

class ReceiversBroadcaster extends Broadcaster {
  val executor = Executors.newCachedThreadPool(new NamedDaemonThreadFactory("StarlingBroadcaster"))
  val receivers = new java.util.concurrent.ConcurrentHashMap[AnyRef,Receiver]()
  def addReceiver(ref:AnyRef, receiver:Receiver) {
    receivers.put(ref, receiver)
  }
  def removeReceiver(ref:AnyRef) {
    receivers.remove(ref)
  }
  def broadcast(event: Event) = {
    import JavaConversions._
    receivers.values().iterator().foreach { receiver => {
      executor.execute(new Runnable() { def run() { receiver.event(event) } })
    }}
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