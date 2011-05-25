package starling.utils

import net.sf.cglib.proxy.{MethodProxy, MethodInterceptor, Enhancer}
import java.lang.reflect.{InvocationTargetException, Method}

/**
 * Changes the thread name to reflect the method being invoked.
 * Should make debugging easier.
 */
object ThreadNamingProxy {
  def proxy[T](wrapped:T, klass:Class[T]):T = {
    val prefix = klass.getName.substring(klass.getName.lastIndexOf(".")+1) + "#"
    val e = new Enhancer();
    e.setSuperclass(klass)
    e.setCallback(new MethodInterceptor() {
      def intercept(obj: Object, method: Method,
                    args: Array[Object], proxy: MethodProxy): Object = {
        val originalName = Thread.currentThread.getName
        try {
          try {
            Thread.currentThread.setName(originalName + " > " + prefix + method.getName)
            method.invoke(wrapped, args :_* )
          } catch {
            case e:InvocationTargetException => throw e.getCause
          }
        } finally {
          Thread.currentThread.setName(originalName)
        }
      }
    })
    e.create().asInstanceOf[T]
  }
}