package starling.webservice

import net.liftweb.json._
import starling.daterange.Day
import org.joda.time.LocalDate
import scala.Any
import net.liftweb.json.JsonAST.JValue
import starling.quantity.Quantity
import starling.quantity.UOM
import starling.quantity.UOM._

object EDMFormats extends DefaultFormats {
  override val typeHintFieldName = "type"
  override val typeHints = EDMHints

  private val localDateSerializer = new Serializer[LocalDate] {
    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case (localDate: LocalDate) => JString(Day.fromLocalDate(localDate).toString)
    }

    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), LocalDate] = {
      case (typeInfo, JString(Day(day))) => day.toLocalDate
    }
  }

  private val daySerializer = new Serializer[Day] {
    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case (day: Day) => JString(day.toString)
    }

    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Day] = {
      case (typeInfo, JString(Day(day))) => day
    }
  }

  val quantitySerializer = new Serializer[Quantity] {
    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case (q : Quantity) => JString(q.toString)
    }

    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Quantity] = {
      case (typeInfo, JString(Quantity.Parse(q))) => q
    }
  }
  override val customSerializers = List(localDateSerializer, daySerializer, quantitySerializer)

  object EDMHints extends TypeHints {
    def classFor(hint: String) = Some(Class.forName(hint))
    def hintFor(clazz: Class[_]) = clazz.getName
    val hints = Nil
    override def containsHint_?(clazz: Class[_]) = true
  }
}
