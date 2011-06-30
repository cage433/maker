package starling.services.rpc

import net.liftweb.json._

import RichJValue._


object JsonDeserializer {
  def deserialize(text: String)(implicit formats: Formats): Any = extract(JsonParser.parse(text).uncapitalize)
  def pretty(text: String)(implicit formats: Formats): String = Printer.compact(render(JsonParser.parse(text)))

  private def extract(json: JValue)(implicit formats: Formats): Any =
    Extraction.extract(json, TypeInfo(classFrom(json, formats.typeHintFieldName), None))

  private def classFrom(value: JValue, TypeHintFieldName: String): Class[_] = value match {
    case JObject(JField(TypeHintFieldName, JString(className)) :: _) => Class.forName(className)
    case _ => throw new Exception("Could not obtain type information from: " + value)
  }
}