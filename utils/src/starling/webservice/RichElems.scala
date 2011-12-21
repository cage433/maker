package starling.webservice

import net.liftweb.json._
import starling.utils.ImplicitConversions._
import net.liftweb.json
import starling.utils.{Double, XmlUtil}
import xml._
import scalaz.Scalaz._
import transform.{RewriteRule, RuleTransformer}

trait RichElems {
  import RichJValue._

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

    def transform(f: Function[Node, Node]) = {
      val transformer = new RuleTransformer(new RewriteRule {
        override def transform(node: Node) = f(node)
      })

      transformer.transform(elem).head
    }
  }
}

