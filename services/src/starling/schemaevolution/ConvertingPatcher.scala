package starling.schemaevolution

import starling.instrument.utils.StarlingXStream
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._
import starling.utils.ClosureUtil._
import xml._

trait Convertable[To] {
  def convert: To
}

class ConvertingPatcher[From <: Convertable[To] : Manifest, To: Manifest](fromClassName: String, toClassName: String) {
  def this(fromClassName: String) = this(fromClassName, fromClassName)

  final def patch(xml: String): String = xml.contains(fromClassName) ? patch(XML.loadString(xml)).toString | xml
  final def patch(elem: Elem): Elem = elem.transform(transformNode).asInstanceOf[Elem]
  def predicate(from: From): Boolean = true

  private def transformNode(node: Node): Node = fromXml(node).fold(c => toXml(c.convert, node), node)

  private def fromXml(maybeFrom: Node): Option[From] = if (!maybeFrom.isInstanceOf[Elem]) None else {
    safely(fromStream.fromXML(maybeFrom.toString()).asInstanceOf[From]).toOption.filter(predicate)
  }

  private def toXml(to: To, original: Node): Elem = XML.loadString(toStream.toXML(to)) |> { elem => original.label match {
    case `toClassName` => elem
    case other => elem.copy(label = other, attributes = elem.attributes.append(toAttribute))
  } }

  private val fromStream = StarlingXStream.createXStream.update(_.alias(fromClassName, manifest[From].erasure))
  private val toStream = StarlingXStream.createXStream.update(_.alias(toClassName, manifest[To].erasure))
  private val toAttribute: MetaData = new UnprefixedAttribute("class", toClassName, scala.xml.Null)
}