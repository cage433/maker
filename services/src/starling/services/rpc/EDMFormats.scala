package starling.services.rpc

import net.liftweb.json.JsonAST.JValue
import net.liftweb.json._
import starling.daterange.Day
import org.joda.time.LocalDate

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

  override val customSerializers = List(localDateSerializer)
}

object EDMHints extends TypeHints {
  def classFor(hint: String) = Some(Class.forName(hint))
  def hintFor(clazz: Class[_]) = clazz.getName
  val hints = Nil
  override def containsHint_?(clazz: Class[_]) = true
}