package starling.utils

import java.lang.reflect.Method

object ReflectionEquals {
  private def isFieldAccessor(method:Method) = {
    val bla =
      method.getName.contains("copy") || method.getName.contains("apply") ||
        method.getName.contains("curried") || method.getName.contains("curry") ||
        method.getName.contains("tupled") || method.getName.toLowerCase.contains("cache") ||
        method.getName.contains("product") || method.getName.contains("hashCode")
    !bla && method.getParameterTypes.isEmpty
  }

  case class Diff(field: Method, aa: AnyRef, bb: AnyRef) {

    override def toString =
      "Differences for getter method '%s'\n1: '%s'\n2: '%s'".format(field, aa, bb)
  }

  def fieldsThatDiffer[A<:AnyRef, B<:AnyRef](a: A, b: B) = {
    val fields = a.getClass.getDeclaredMethods.filter(isFieldAccessor)
    fields.flatMap {
      case field => {
        val aa = field.invoke(a)
        val bb = field.invoke(b)
        if (aa != bb) {
          Some(Diff(field, aa, bb))
        } else {
          None
        }
      }
    }
  }
}
