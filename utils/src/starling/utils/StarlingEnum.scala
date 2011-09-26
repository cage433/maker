package starling.utils

import java.lang.reflect.Method

import starling.utils.ImplicitConversions._
import starling.utils.Pattern._
import scalaz._
import Scalaz._
import java.util.concurrent.atomic.AtomicInteger

/**
 * StarlingEnum defines a base class from which enumerated types in Starling should be derived.  It provides overloaded
 * look-up functions to retrieve each instance of the enumerated type by its (optionally case insensitive) name.
 *
 * Some subtleties of the implementation being that:
 * - Only lazy vals may reference its values, from-name look-up functions or sort indices; and
 * - Each instance _may_ provide a list of other types associated with it.
 *
 * @documented
 */
class StarlingEnum[T](theType:Class[T], namer: T => String, ignoreCase: Boolean = false, otherTypes: List[Class[_]] = Nil) {
  val types = theType :: otherTypes
  lazy val values:List[T] = try {
    getClass.getDeclaredMethods.filter(isFieldAccessor).map(method => method.invoke(this).update {
      v => {
        if (v==null) throw new Exception("Field " + method.getName + " is null. Only lazy vals can reference StarlingEnum.values / fromName / sortIndex")
      }
    }.asInstanceOf[T]).toList
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

  protected def toCase(name: String) = if (ignoreCase) name.toLowerCase else name
  private def isFieldAccessor(method: Method) = {
    val assignable = types.find(_ == method.getReturnType).isDefined
    assignable && method.getParameterTypes.isEmpty
  }
  private def throwUnknown(name: String): T = throw new Exception("Unknown " + this.getClass.getSimpleName + ": " + name)
}
