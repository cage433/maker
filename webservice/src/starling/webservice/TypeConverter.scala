package starling.webservice

import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl
import java.lang.reflect.{ParameterizedType, Type}
import java.util.ArrayList
import starling.utils.ImplicitConversions._
import org.jboss.resteasy.spi.StringConverter
import scala.collection.JavaConversions._
import scalaz.Scalaz._


trait TypeConverter {
  val baseType: Class[_]
  val genericType: Type

  def convert(value: AnyRef): AnyRef
}

object TypeConverter {
  def apply(`type`: Class[_], genericType: Type) = if (`type` == classOf[Option[_]])
    OptionTypeConverter(listClass, parameterizedList(genericType)) else NullTypeConverter(`type`, genericType)

  private def parameterizedList(genericType: Type): ParameterizedType =
    ParameterizedTypeImpl.make(listClass, genericType.asInstanceOf[ParameterizedType].getActualTypeArguments, null)

  private val listClass = classOf[java.util.List[_]]
}

case class OptionTypeConverter(baseType: Class[_], genericType: Type) extends TypeConverter {
  def convert(value: AnyRef) = toList(value).headOption.flatMap { _ match {
    case option:Option[_] => option
    case element => Some(element)
  } }

  private def toList(value: AnyRef): List[_] = (value.cast[ArrayList[_]] | new ArrayList()).toList
}

case class NullTypeConverter(baseType: Class[_], genericType: Type) extends TypeConverter {
  def convert(value: AnyRef) = value
}

object OptionStringConverter extends StringConverter[Option[_]] {
  def fromString(str: String) = throw new Exception("Not implemented")
  def toString(option: Option[_]) = toArrayList(option).toString

  private def toArrayList(option: Option[_]) = new ArrayList[Any] |> (list => option.foreach(value => list.add(value)))
}