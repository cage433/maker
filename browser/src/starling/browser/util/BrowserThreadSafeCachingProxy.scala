package starling.browser.util

import java.lang.reflect.{InvocationTargetException, UndeclaredThrowableException, InvocationHandler, Method, Proxy}

/**
 * A copy of ThreadSafeCachingProxy in utils. But Browser only depends on 3 classes in utils so it has its own copy
 *
 * Caches the result of all method calls
 */
object BrowserThreadSafeCachingProxy{
  case class MethodInvocation(method:Method, args:List[Any])
  private val cache = new BrowserCache
  def createProxy[T](original:T, klass:Class[T]) = {
    Proxy.newProxyInstance(
      BrowserThreadSafeCachingProxy.getClass.getClassLoader,
      Array(klass),
      new InvocationHandler {
        def invoke(proxy: Object, method: Method, args: Array[Object]) = {
          val methodInvocation = MethodInvocation(method, if (args==null) List() else args.toList)
          try {
            try {
              cache.memoize(methodInvocation) { method.invoke(original, args: _ *) }
            } catch {
              case e: UndeclaredThrowableException => throw e.getUndeclaredThrowable
              case e: InvocationTargetException => throw e.getTargetException
            }
          } catch {
            case e => {
              cache.remove(methodInvocation)
              throw e
            }
          }
        }
      }
    ).asInstanceOf[T]
  }

  /**
   * Clears the cache. This is only really used for dev.
   */
  def clearCache {cache.clear}
}
