package starling.utils.ref

import java.lang.{reflect => jreflect}
import java.lang.reflect.Method

/**
 * Scala counterpart of java.lang.reflect.InvocationHandler
 */
trait InvocationHandler {
  def invoke(proxy:AnyRef, method:Method, args:Array[AnyRef]):AnyRef
}

object ScalaProxy {
  def apply[T <: AnyRef](handler:InvocationHandler)(implicit manifest:Manifest[T]):T = {
    val clazz = manifest.erasure
    val h = new Handler(handler)
    jreflect.Proxy.newProxyInstance(clazz.getClassLoader(), Array(clazz), h).asInstanceOf[T]
  }

  private class Handler(handler:InvocationHandler) extends jreflect.InvocationHandler {
    def invoke(proxy:Object, method:jreflect.Method, args:Array[Object]):Object = {
      try {
        handler.invoke(proxy, method, args)
      } catch {
        case e:jreflect.InvocationTargetException =>
          throw e.getTargetException
        case e:jreflect.UndeclaredThrowableException =>
          throw e.getUndeclaredThrowable
        case e =>
          throw e
      }
    }
  }

}

trait Foo {
  def foo(x:Int, y:String): String

  def bar_!():String
}

object Test extends App {

  class Conc extends Foo {
    def foo(x:Int, y:String) = y + x

    def bar_!() = "bla"
  }
  val c = new Conc

  val h = new InvocationHandler {
    def invoke(proxy:AnyRef, method:Method, args:Array[AnyRef]) = {
      method.invoke(c, args: _*)
    }
  }
  val p = ScalaProxy[Foo](h)
  println("? " + p.foo(1, "str"))
  println("? " + p.bar_!())
  //prints:
  //called foo(x: Int = 1, y: String = str): Unit
  //called bar_!(): Unit
}