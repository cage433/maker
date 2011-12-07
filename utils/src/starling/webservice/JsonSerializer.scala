package starling.webservice

import net.liftweb.json._

import RichJValue._
import java.lang.String


case class JsonSerializer()(implicit formats: Formats) {
  def serialize(value: Any): String = Printer.compact(render(toJValue(value)))
  def pretty(value: Any): String = Printer.pretty(render(toJValue(value)))
  def toJValue(value: Any): JValue = Extraction.decompose(value).capitalize
}

object JsonSerializer {
  def serialize[T](value: T)(implicit cm: ClassManifest[T], formats: Formats) = JsonSerializer().serialize(value)
}
