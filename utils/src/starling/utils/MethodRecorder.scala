package starling.utils

import java.lang.reflect.Method
import net.sf.cglib.proxy.{MethodProxy, MethodInterceptor, Enhancer}

/**
 * Wraps an object and records all method invocations
 */
class MethodRecorder[T](wrapped:T, klass:Class[_]) {
  val record = new scala.collection.mutable.HashSet[(String,List[Object],Object)]()
  val proxy:T = {
    val e = new Enhancer();
    e.setSuperclass(klass)
    e.setCallback(new MethodInterceptor() {
      def intercept(obj: Object, method: Method,
                    args: Array[Object], proxy: MethodProxy): Object = {
        val result = method.invoke(wrapped, args :_* )
        record += ( (method.getName, if (args == null) List() else args.toList, result))
        result
      }
    })
    e.create().asInstanceOf[T]
  }

  /**
   * Same as proxy above but takes a param that also gets called for every method call
   */
  def proxyWithIntercept(m: (String, List[Object], Object) => Unit):T = {
    val e = new Enhancer();
    e.setSuperclass(klass)
    e.setCallback(new MethodInterceptor() {
      def intercept(obj: Object, method: Method,
                    args: Array[Object], proxy: MethodProxy): Object = {
        val result = method.invoke(wrapped, args :_* )
        val argsList = if (args == null) List() else args.toList
        record += ( (method.getName, argsList, result))
        m(method.getName, argsList, result)
        result
      }
    })
    e.create().asInstanceOf[T]
  }
}