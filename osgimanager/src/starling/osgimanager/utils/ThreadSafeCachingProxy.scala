package starling.osgimanager.utils

import java.lang.reflect.{Method, InvocationHandler, Proxy, UndeclaredThrowableException, InvocationTargetException}
import starling.manager.Memoize
import starling.utils.cache.CacheFactory

/**
 * Caches the result of all method calls unless they have the annotation @Memoize
 */
object ThreadSafeCachingProxy{
  case class MethodInvocation(klass:String, method:String, args:List[Any])

  def createProxy[T](klass:Class[T], original:T) = {
    if (!klass.isInterface) {
      original
    } else {
      val cache = CacheFactory.getCache("ThreadSafeCachingProxy:"+klass.getName, unique = true)
      Proxy.newProxyInstance(
        klass.getClassLoader,
        Array(klass),
        new InvocationHandler {
          def invoke(proxy: Object, method: Method, args: Array[Object]) = {
            def invokeMethod() = { method.invoke(original, args: _ *) }
            try {
              if (method.getAnnotation(classOf[Memoize]) != null) {
                val methodInvocation = MethodInvocation(method.getDeclaringClass.getName, method.getName, if (args == null) List() else args.toList)
                try {
                  cache.memoize(methodInvocation, invokeMethod())
                }
              } else {
                invokeMethod()
              }
            } catch {
              case e: Throwable => throw rootCause(e)
            }
          }
        }
      ).asInstanceOf[T]
    }
  }

  private def rootCause(e: Throwable) = {
    def rec(e: Throwable, depth: Int): Throwable = if (depth < 5) {
      e match {
        case i: InvocationTargetException => rec(i.getCause, depth + 1)
        case i: UndeclaredThrowableException => rec(i.getCause, depth + 1)
        case _ => e
      }
    } else {
      e
    }
    val cause = rec(e, 0)
    cause
  }
}
