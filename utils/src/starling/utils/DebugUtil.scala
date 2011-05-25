package starling.utils

import collection.mutable.{HashMap, ListBuffer}
import java.util.concurrent.Executors
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.collection.JavaConversions._

object DebugUtil {
  private lazy val util = new DebugUtil

  def count = util.count
  def track[T <: AnyRef, P](value : T)(finder : T => P => Boolean)(implicit mv : Manifest[T]) = util.track(value, finder)
}

class DebugUtil {
  import ClosureUtil._

  private val tracking = new AtomicBoolean(false)
  private val tracked: HashMap[Class[_], Tracked] = new HashMap()
  private val trackedCount = new AtomicInteger(0)
  private val currentCount = new AtomicInteger(0)

  startDaemon(() => forever {
    displayTracked
    Thread.sleep(500)
  })

  private def displayTracked = {
    println("Tracking: " + trackedCount + ", objects")
  }

  def track[T <: AnyRef, P](value : T, finder : T => P => Boolean)(implicit mv : Manifest[T]) = if (tracking.get) {
    tracked(mv.erasure, finder).append(value)
    trackedCount.incrementAndGet
  }

  def start = tracking.set(true)
  def stop = tracking.set(false)

  def clear {
    tracked.clear
    trackedCount.set(0)
  }

  def count = currentCount.getAndIncrement


  def tracked[T <: AnyRef, P](trackedClass : Class[_], finder : T => P => Boolean): Tracked =
    tracked.getOrElseUpdate(trackedClass, Tracked(finder))

  def find(property : Any) : java.util.List[AnyRef] = tracked.values.flatMap(owner => owner.find(property)).toList

  object Tracked {
    def apply[T <: AnyRef,P](finder : T => P => Boolean) : Tracked = new Tracked(flip(untyped(finder)))

    def untyped[T <: AnyRef,P](finder : T => P => Boolean) : AnyRef => Any => Boolean =
      owner => property => invoke(finder, owner, property)

    def invoke[T <: AnyRef,P](finder : T => P => Boolean, owner: AnyRef, property: Any): Boolean = {
      if (owner.isInstanceOf[T] && property.isInstanceOf[P]) {
        val towner: T = owner.asInstanceOf[T]
        val tproperty: P = property.asInstanceOf[P]

        finder(towner)(tproperty)
      }
      else {
        false
      }
    }

    def flip(finder : AnyRef => Any => Boolean) : Any => AnyRef => Boolean = property => owner => finder(owner)(property)
  }

  class Tracked(finder : Any => AnyRef => Boolean) {
    private var tracked = new ListBuffer[TrackedItem]

    def append(owner : AnyRef): Unit = tracked.append(TrackedItem(owner))
    def find(property : Any): ListBuffer[TrackedItem] = tracked.filter(itemFinder(property))
    def itemFinder(property : Any) : TrackedItem => Boolean = trackedItem => finder(property)(trackedItem.item)
  }

  case class TrackedItem(item : AnyRef) {
    val stackTrace = try { throw new Exception() } catch { case e => e }
  }

}

