package starling.services.rpc

import net.liftweb.json._

object RichJValue {
  implicit def enrichJValue(jvalue: JValue) = new RichJValue(jvalue)

  class RichJValue(jvalue: JValue) {
    def mapFieldNames(f: String => String): JValue = jvalue.map { value => value match {
      case JObject(fields) => JObject(fields.map(field => JField(f(field.name), field.value.mapFieldNames(f))))
      case jarray: JArray => jarray.map(_.mapFieldNames(f))
      case other => other
    } }
  }
}