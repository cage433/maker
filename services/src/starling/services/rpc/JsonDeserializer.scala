package starling.services.rpc

import net.liftweb.json.TypeInfo._
import net.liftweb.json._

import RichJValue._
import starling.utils.ImplicitConversions._


object JsonDeserializer {
  def pretty(text: String)(implicit formats: Formats): String = Printer.compact(render(JsonParser.parse(text)))

  def deserialize(text: String)(implicit formats: Formats): Any = {
    val json = JsonParser.parse(text)
    val ujson = uncapitalize(json)

    Extraction.extract(ujson, TypeInfo(classFrom(ujson), None))
  }

  private def classFrom(jvalue: JValue): Class[_] = jvalue match {
    case JObject(fields) => {
      val typeValues = fields.find(_.name == "type").getOrElse(throw new Exception("Missing type information")).value

      typeValues match {
        case JString(className) => Class.forName(className)
        case _ => throw new Exception("Missing type information")
      }
    }
  }

  private def uncapitalize(input: JValue): JValue = input.mapFieldNames(_.uncapitalize)
}
