package starling.browser.util

import com.google.common.collect.MapMaker
import java.util.concurrent.{Callable, FutureTask}

/**
 * Mostly copied from SimpleCache in utils but Browser only depends on 3 utils classes so they are copied
 */
class BrowserCache {

  private val cache = new MapMaker().concurrencyLevel(16).softValues().makeMap[Object, FutureTask[Object]]

  def memoize[K, V](key: K)(f: => V): V = putIfAbsent(key, f)

  def clear {cache.clear}

  def remove[K](key: K) = cache.remove(key)

  /**
   * Changes a cached stacktrace to look as it would do were there no memoization
   */
  private def fixStackTrace(e : Throwable, originalStacktrace : Array[StackTraceElement]) : Throwable = {
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

  def putIfAbsent[K,V](k: K, f: => V): V = {
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
    var actualTask = cache.putIfAbsent(k.asInstanceOf[Object], task.asInstanceOf[FutureTask[Object]])
    if (actualTask == null) { //null indicates there was no previous entry for key
      actualTask = task.asInstanceOf[FutureTask[Object]]
      task.run()
    }
    getValueOrThrowExceptionWithFixedStackTrace(actualTask)
  }
}
