package starling.utils.cache

import java.util.concurrent.{Callable, FutureTask, ConcurrentHashMap}
import java.lang.reflect.{Method, InvocationHandler, Proxy, UndeclaredThrowableException, InvocationTargetException}
import starling.utils.Log

/**
 * Caches the result of all method calls
 */
object ThreadSafeCachingProxy{
  case class MethodInvocation(method:Method, args:List[Any])
  private val cache = CacheFactory.getCache("ThreadSafeCachingProxy")
  def createProxy[T](original:T, klass:Class[T]) = {
    Proxy.newProxyInstance(
      ThreadSafeCachingProxy.getClass.getClassLoader,
      Array(klass),
      new InvocationHandler {
        def invoke(proxy: Object, method: Method, args: Array[Object]) = {
          val methodInvocation = MethodInvocation(method, if (args==null) List() else args.toList)
          try {
            try {
              cache.memoize(methodInvocation, method.invoke(original, args: _ *))
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


class ThreadSafeCachingProxyInstance(val name : String){
  case class MethodInvocation(method:Method, args:List[Any])
  private val cache = CacheFactory.getCache(name)
  def createProxy[T](original:T, klass:Class[T]) = {
    cache.clear
    Proxy.newProxyInstance(
      ThreadSafeCachingProxy.getClass.getClassLoader,
      Array(klass),
      new InvocationHandler {
        def invoke(proxy: Object, method: Method, args: Array[Object]) = {
          val methodInvocation = MethodInvocation(method, if (args==null) List() else args.toList)
          try {
            cache.memoize(methodInvocation, method.invoke(original, args : _ *))
          } catch {
//            case e : UndeclaredThrowableException => throw e.getUndeclaredThrowable
            case e : InvocationTargetException => throw e.getTargetException
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