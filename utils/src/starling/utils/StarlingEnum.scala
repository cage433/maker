package starling.utils

import java.lang.reflect.Method

import starling.utils.ImplicitConversions._
import starling.utils.Pattern._
import scalaz.Scalaz._


class StarlingEnum[T](theType:Class[T], namer: T => String, ignoreCase: Boolean = false, otherTypes: List[Class[_]] = Nil) {
  val types = theType :: otherTypes
  lazy val values:List[T] = try {
    getClass.getDeclaredMethods.filter(isFieldAccessor).map(method => method.invoke(this).update { v => {
      if (v==null) throw new Exception("Field " + method.getName + " is null. Only lazy vals can reference StarlingEnum.values / fromName / sortIndex")
    } }.asInstanceOf[T]).toList
  } catch {
    case e => throw new Exception("Problem loading values in StarlingEnum", e)
  }
  lazy val names = values.map(namer)
  lazy val sortIndex = values.zipWithIndex.toMap
  private lazy val valuesByName = values.toMapWithKeys(v => toCase(namer(v)))

  def fromName(name: String): T = find(name).getOrElse(throwUnknown(name))
  def fromName(name: Option[String]): Option[T] = name.map(fromName)
  def find(name: String) = valuesByName.get(toCase(name))
  val Parse: Extractor[Any, T] = Extractor.from[Any](value => find((value ?? "").toString))

  protected def toCase(name: String) = (if (ignoreCase) name.toLowerCase else name).trim
  private def isFieldAccessor(method: Method) = {
    val assignable = types.find(_ == method.getReturnType).isDefined
    assignable && method.getParameterTypes.isEmpty
  }
  private def throwUnknown(name: String): T = throw new Exception("Unknown %s: '%s'" % (this.getClass.getSimpleName, name))
}