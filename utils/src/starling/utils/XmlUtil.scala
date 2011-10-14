package starling.utils

import starling.utils.ImplicitConversions._
import collection.Traversable
import xml._

object XmlUtil {
  def elementToAttribute[N <: Node](xml: N): N = xml transform {
    case elem: Elem => extractAttributes(elem) |> { (nodes, attributes) =>
      elem.copy(child = nodes.map(elementToAttribute(_)), attributes = attributes.append(elem.attributes)).asInstanceOf[N]
    }
  }

  def attributeToElement[N <: Node](xml: N): N = xml transform {
    case elem: Elem => elem.copy(attributes = Null, child = toElements(elem.attributes) ::: elem.child.toList).asInstanceOf[N]
  }

  private def extractAttributes(xml: Node): (List[Node], MetaData) =
    xml.child.toList.partitionEithers(elementsWithText(_)) |> ((nodes, attributes) => (nodes.toList, join(attributes)))

  private def join(attributes: Traversable[MetaData]) = (Null.asInstanceOf[MetaData] /: attributes.toList.reverse)(_ append _)

  private def elementsWithText(xml: Node): Either[Node, MetaData] = xml match {
    case Elem(prefix, label, _, _, Text(text)) => Right(new PrefixedAttribute(prefix, label, text, Null))
    case other => Left(other)
  }

  private def toElements(attribute: MetaData): List[Node] = attribute match {
    case PrefixedAttribute(prefix, label, value, next) => Elem(prefix, label, Null, TopScope, value : _*) :: toElements(next)
    case UnprefixedAttribute(label, value, next) => Elem(null, label, Null, TopScope, value : _*) :: toElements(next)
    case _ => Nil
  }
}