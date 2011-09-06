package starling.utils

import java.util.concurrent.Executors
import swing.event.Event
import scala.collection.JavaConversions

import collection.immutable.List
import ClosureUtil._
import ImplicitConversions._
import collection.mutable.ListBuffer


trait Broadcaster {
  def broadcast(event: Event)
}

object Broadcaster {
  val Null = new Broadcaster() { def broadcast(event: Event) {} }
}

class CompositeBroadcaster(broadcasters: (Boolean, Broadcaster)*) extends Broadcaster {
  private val enabledBroadcasters = broadcasters.filter(_._1).map(_._2)

  def broadcast(event: Event) = enabledBroadcasters.map(_.broadcast(event))
}

abstract class TypedBroadcaster[T](implicit manifest: Manifest[T]) extends Broadcaster {
  override def broadcast(event: Event) = manifest.safeCast(event).foreach(typedBroadcast)

  def typedBroadcast(t: T)
}

case class ObservingBroadcaster(broadcaster: Broadcaster) extends Broadcaster {
  private val observers = new ListBuffer[(Event) => Any]()

  def +=(observer: (Event) => Any) = { observers += observer; this }

  def broadcast(event: Event) = {
    observers.foreach(_(event))
    broadcaster.broadcast(event)
  }
}

