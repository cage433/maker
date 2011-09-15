package starling.utils

import net.sf.cglib.proxy.{MethodProxy, MethodInterceptor, Enhancer}
import java.lang.reflect.{InvocationTargetException, Method}

/**
 * Changes the thread name to reflect the method being invoked.
 * Should make debugging easier.
 */
object ThreadNaming {
  def proxy[T](wrapped:T, klass:Class[T]):T = {
    val prefix = klass.getName.substring(klass.getName.lastIndexOf(".")+1) + "#"
    val e = new Enhancer();
    e.setSuperclass(klass)
    e.setCallback(new MethodInterceptor() {
      def intercept(obj: Object, method: Method,
                    args: Array[Object], proxy: MethodProxy): Object = {
        withNamedThread(prefix + method.getName) {
          try {
            method.invoke(wrapped, args :_* )
          } catch {
            case e:InvocationTargetException => throw e.getCause
          }
        }
      }
    })
    e.create().asInstanceOf[T]
  }

  def withNamedThread[T <: Object](name:String)(f: => T):T = {
    val originalName = Thread.currentThread.getName
    try {
      Thread.currentThread.setName(originalName + " > " + name)
      f
    } finally {
      Thread.currentThread.setName(originalName)
    }
  }
}