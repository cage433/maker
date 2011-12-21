package starling.utils.cache

import com.google.common.collect.MapMaker
import starling.utils.CollectionUtils._
import java.util.concurrent.atomic.{AtomicLong, AtomicInteger}
import java.lang.String
import starling.utils.cache.CacheFactory.{StatsListener, Stats, Cache, CacheImpl}
import java.util.concurrent._
import collection.JavaConversions._

class SimpleCacheImpl(val soft: Boolean) extends CacheImpl {
  private val caches:ConcurrentMap[String, FutureTask[SimpleCache]] = new MapMaker().concurrencyLevel(16).makeMap[String, FutureTask[SimpleCache]]

  def get(name: String, statsListener: StatsListener, unique: Boolean): Cache = {
    if(!unique)
      putIfAbsent(caches, name, new SimpleCache(statsListener, soft))._1
    else
      new SimpleCache(statsListener, soft)
  }
  def clear{
    if (soft)
      caches.clear
    else
      throw new Exception("Won't clear a non-soft cache what you really wanted?")
  }
}

class SimpleCache(statsListener: StatsListener, soft: Boolean) extends Cache(statsListener) {
  private val cache = if (soft) {
    new MapMaker().concurrencyLevel(16).softValues().makeMap[Object, FutureTask[Object]]
  } else {
    new MapMaker().concurrencyLevel(16).makeMap[Object, FutureTask[Object]]
  }


  import scala.collection.JavaConversions._
  def keys[K]() = Set[K]() ++ (scala.collection.mutable.Set[K]() ++ cache.asInstanceOf[ConcurrentMap[K, FutureTask[_]]].keySet)
  
  def memoize[K, V](key: K, f: => V): V = {
    putIfAbsent(key, f) match {
      case (v, true) => {
        statsListener.hit
        v
      }
      case (v, false) => {
        statsListener.miss
        v
      }
    }
  }



  /**
   * Changes a cached stacktrace to look as it would do were there no memoization
   */
  def fixStackTrace(e : Throwable, originalStacktrace : Array[StackTraceElement]) : Throwable = {
    e.setStackTrace(originalStacktrace ++ Thread.currentThread.getStackTrace.drop(1))
    e
  }

  private def getValueOrThrowExceptionWithFixedStackTrace[V](ft : FutureTask[Object]) : V = {
    ft.get.asInstanceOf[Either[V, (Throwable, Array[StackTraceElement])]] match {
      case Left(v) => v
      case Right((e, stacktrace)) => {
        throw fixStackTrace(e, stacktrace)
      }
    }
  }

  val sequence = new java.util.concurrent.atomic.AtomicInteger(0)

  def putIfAbsent[K,V](k: K, f: => V): (V, Boolean) = {
    val task = new FutureTask(new Callable[Either[V, (Throwable, Array[StackTraceElement])]]() {
      def call = {
        try {
          Left(f)
        } catch {
          case e : Throwable => {
            // Save the stack trace between the point the error was thrown and here
            // so that fixStackTrace can do its thing
            val exceptionStackTrace = e.getStackTrace
            val thisThreadsStackTrace = Thread.currentThread.getStackTrace
            Right((e, exceptionStackTrace.dropRight(thisThreadsStackTrace.size)))
          }
        }
      }
    })
    var hit = true
    var actualTask = cache.putIfAbsent(k.asInstanceOf[Object], task.asInstanceOf[FutureTask[Object]])
    if (actualTask == null) { //null indicates there was no previous entry for key
      hit = false
      actualTask = task.asInstanceOf[FutureTask[Object]]
      task.run()
    }
    (getValueOrThrowExceptionWithFixedStackTrace(actualTask), hit)
  }


  def getIfComplete[K, V](key: K): Option[V] = {
    cache.get(key) match {
      case null => {
        statsListener.miss
        None
      }
      case ft => {
        statsListener.hit
        if (ft.isDone) {
          Some(getValueOrThrowExceptionWithFixedStackTrace(ft))
        } else
          None
      }
    }
  }

  def get[K, V](key: K): Option[V] = {
    cache.get(key) match {
      case null => {
        statsListener.miss
        None
      }
      case ft => {
        statsListener.hit
        Some(getValueOrThrowExceptionWithFixedStackTrace(ft))
      }
    }
  }

  def clear {cache.clear}

  def remove[K](key: K) = cache.remove(key)
}
