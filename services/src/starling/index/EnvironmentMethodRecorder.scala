package starling.index

import java.lang.reflect.Method
import net.sf.cglib.proxy.{MethodProxy, MethodInterceptor, Enhancer}

class EnvironmentMethodRecorder[T](wrapped: T, klass: Class[_]) {
  val record = new scala.collection.mutable.HashSet[(String, List[Object], Object)]()

  def proxyWithIntercept(m: (String, List[Object], Object) => Unit): T = {
    val e = new Enhancer()
    e.setSuperclass(klass)
    e.setCallback(new MethodInterceptor() {
      def intercept(enhancedObject: Object, originalMethod: Method,
                    args: Array[Object], enhancedMethod: MethodProxy): Object = {
        def resultOnSelf = enhancedMethod.invokeSuper(enhancedObject, args)

        val argsList = if (args == null) List() else args.toList

        originalMethod.getName match {
          case "instrumentLevelEnv" | "environmentParameters" => {
            originalMethod.invoke(wrapped, args: _*)
          }
          case name => {
            val result = resultOnSelf
            record += ((originalMethod.getName, argsList, result))
            m(originalMethod.getName, argsList, result)
            result
          }
        }
      }
    })
    e.create().asInstanceOf[T]
  }
}