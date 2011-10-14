package starling.webservice

import net.liftweb.json._
import starling.daterange.Day
import org.joda.time.LocalDate
import edu.oswego.cs.dl.util.concurrent.FJTask.Par
import scala.Any
import net.liftweb.json.JsonAST.JValue

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

  override val customSerializers = List(localDateSerializer, daySerializer)

  object EDMHints extends TypeHints {
    def classFor(hint: String) = Some(Class.forName(hint))
    def hintFor(clazz: Class[_]) = clazz.getName
    val hints = Nil
    override def containsHint_?(clazz: Class[_]) = true
  }
}