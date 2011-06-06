package starling.utils

import starling.utils.ImplicitConversions._
import java.lang.reflect.Method

trait Named {
  def name(): String
}

class StarlingEnum[T <: Named](theType:Class[T], ignoreCase: Boolean = false) {
  lazy val values:List[T] = {
    getClass.getDeclaredMethods.filter(isFieldAccessor).map(method => method.invoke(this).update {
      v => {
        if (v==null) throw new Exception("Field " + method.getName + " is null. Only lazy vals can reference StarlingEnum.values / fromName / sortIndex")
      }
    }.asInstanceOf[T]).toList
  }
  lazy val sortIndex = values.zipWithIndex.toMap
  private lazy val valuesByName = values.toMapWithKeys(v => toCase(v.name))

  def fromName(name: String) = find(name).getOrElse(throwUnknown(name))
  def fromName(name: Option[String]): Option[T] = name.map(fromName)
  def find(name: String) = valuesByName.get(toCase(name))

  private def toCase(name: String) = if (ignoreCase) name.toLowerCase else name
  private def isFieldAccessor(method: Method) = method.getReturnType == theType && method.getParameterTypes.isEmpty
  private def throwUnknown(name: String): T = throw new Exception("Unknown " + this.getClass.getSimpleName + ": " + name)
}