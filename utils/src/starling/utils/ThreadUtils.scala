package starling.utils

import net.sf.cglib.proxy.{MethodProxy, MethodInterceptor, Enhancer}
import java.lang.reflect.{InvocationTargetException, Method}

/**
 * Changes the thread name to reflect the method being invoked.
 * Should make debugging easier.
 */
object ThreadUtils {

  def withNamedThread[T <: Object](name:String)(f: => T):T = {
    val originalName = Thread.currentThread.getName
    try {
      Thread.currentThread.setName(originalName + " > " + name)
      f
    } finally {
      Thread.currentThread.setName(originalName)
    }
  }

  def printNonDaemonThreads {
    Thread.getAllStackTraces.entrySet.toArray(Array[java.util.Map.Entry[Thread,Array[StackTraceElement]]]()).foreach {
      entry => {
        val thread = entry.getKey
        if (!thread.isDaemon && thread.getName != "main") {
          println("Non daemon thread: " + thread.getName)

        }
      }
    }
  }

}