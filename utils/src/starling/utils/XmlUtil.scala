package starling.utils

import xml._
import starling.utils.ImplicitConversions._

object XmlUtil {
  def elementToAttribute[N <: Node](xml: N): N = (xml match {
    case elem: Elem => {
      val (rest, textContainingChildren) = elem.child.map(elementsWithText(_)).partitionEithers

      elem.copy(child = rest.map(elementToAttribute(_)), attributes = toAttributes(textContainingChildren).append(elem.attributes))
    }
    case other => other
  }).asInstanceOf[N]

  def attributeToElement[N <: Node](xml: N): N = (xml match {
    case elem: Elem => elem.copy(attributes = Null, child = toElements(elem.attributes) ::: elem.child.toList)
    case other => other
  }).asInstanceOf[N]

  private case class Attribute(prefix: String, label: String, value: String) {
    def appendTo(next: MetaData) = new PrefixedAttribute(prefix, label, value, next)
  }

  private def toAttributes(children: Seq[Attribute]): MetaData = {
    (Null.asInstanceOf[MetaData] /: children)((next: MetaData, child: Attribute) => child.appendTo(next))
  }

  private def elementsWithText(xml: Node) = xml match {
    case Elem(prefix, label, _, _, Text(text)) => Right(Attribute(prefix, label, text))
    case other => Left(other)
  }

  private def toElements(attribute: MetaData): List[Node] = attribute match {
    case PrefixedAttribute(prefix, label, value, next) => Elem(prefix, label, Null, TopScope, value : _*) :: toElements(next)
    case UnprefixedAttribute(label, value, next) => Elem(null, label, Null, TopScope, value : _*) :: toElements(next)
    case _ => Nil
  }
}