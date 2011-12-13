package starling.schemaevolution

import starling.instrument.utils.StarlingXStream
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._
import xml.{Elem, Node, XML}
import starling.utils.ClosureUtil._

trait Convertable[To] {
  def convert: To
}

class ConvertingPatcher[From <: Convertable[To] : Manifest, To: Manifest](className: String) {
  def patch(xml: String): String = xml.contains(className) ? patch(XML.loadString(xml)).toString | xml
  def patch(elem: Elem): Elem = elem.transform(transformNode).asInstanceOf[Elem]

  private def transformNode(node: Node): Node = fromXml(node).fold(c => toXml(c.convert), node)
  private def fromXml(maybeFrom: Node): Option[From] = if (!maybeFrom.isInstanceOf[Elem]) None else {
    safely(fromStream.fromXML(maybeFrom.toString()).asInstanceOf[From]).toOption
  }
  private def toXml(to: To): Elem = XML.loadString(toStream.toXML(to))
  private val fromStream = StarlingXStream.createXStream.update(_.alias(className, manifest[From].erasure))
  private val toStream = StarlingXStream.createXStream.update(_.alias(className, manifest[To].erasure))
}