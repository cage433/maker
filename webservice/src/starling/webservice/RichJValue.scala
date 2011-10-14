package starling.webservice

import net.liftweb.json._
import starling.utils.ImplicitConversions._
import net.liftweb.json
import starling.utils.{Double, XmlUtil}
import xml._
import scalaz.Scalaz._


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

  implicit def enrichElem(elem: Elem) = new RichElem(elem)

  class RichElem(elem: Elem) {
    def toJSON: JValue = {
      def parse(text: String): JValue = text match {
        case Double(double) => JDouble(double)
        case _ => JString(text)
      }

      def field(node: Node): Option[JField] = node partialMatch {
        case Elem(prefix, label, Null, _, Text(text)) => JField((prefix ?? "") + label, parse(text))
        case elem: Elem => JField(elem.label, JObject(fields(elem.attributes) ::: elem.child.toList.flatMap(field(_))))
      }

      def fields(attributes: MetaData): List[JField] = attributes match {
        case PrefixedAttribute(prefix, label, Text(value), next) => JField((prefix ?? "") + label, parse(value)) :: fields(next)
        case UnprefixedAttribute(label, Text(value), next) => JField(label, parse(value)) :: fields(next)
        case _ => Nil
      }

      JObject(XmlUtil.attributeToElement(elem).child.toList.flatMap(field(_))).unhyphenate
    }
  }
}