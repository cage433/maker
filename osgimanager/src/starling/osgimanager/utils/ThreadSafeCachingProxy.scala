package starling.osgimanager.utils

import java.lang.reflect.{Method, InvocationHandler, Proxy, UndeclaredThrowableException, InvocationTargetException}
import starling.manager.DoNotCache
import starling.utils.cache.CacheFactory

/**
 * Caches the result of all method calls unless they have the annotation @DoNotCache
 *
 * @documented
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
            if (method.getAnnotation(classOf[DoNotCache]) != null) {
              invokeMethod()
            } else {
              val methodInvocation = MethodInvocation(method.getDeclaringClass.getName, method.getName, if (args==null) List() else args.toList)
              try {
                try {
                  cache.memoize(methodInvocation, invokeMethod() )
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
        }
      ).asInstanceOf[T]
    }
  }
}
