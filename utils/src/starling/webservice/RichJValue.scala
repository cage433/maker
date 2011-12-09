package starling.webservice

import net.liftweb.json._
import starling.utils.ImplicitConversions._
import net.liftweb.json
import starling.utils.XmlUtil
import xml._


object RichJValue {
  implicit def enrichJValue(jvalue: JValue) = new RichJValue(jvalue)

  class RichJValue(jvalue: JValue) {
    def capitalize(implicit formats: Formats) = jvalue.transform {
      case JObject(fields) if fields.exists(_.name == formats.typeHintFieldName) =>
        JObject(fields.map(jfield => jfield.copy(jfield.name.capitalize)))
    }
    def uncapitalize = jvalue.transform { case jfield: JField => jfield.copy(jfield.name.uncapitalize) }
    def hyphenate    = jvalue.transform { case jfield: JField => jfield.copy(jfield.name.hyphenate)    }
    def unhyphenate  = jvalue.transform { case jfield: JField => jfield.copy(jfield.name.unhyphenate)  }
    def toXml: Elem  = XmlUtil.elementToAttribute(<result>{Xml.toXml(jvalue.hyphenate)}</result>)
    def pretty       = json.pretty(json.render(jvalue))
  }
}