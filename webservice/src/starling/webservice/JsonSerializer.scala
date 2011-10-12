package starling.webservice

import net.liftweb.json._

import RichJValue._
import java.lang.{String, Class}


case class JsonSerializer(clazz: Class[_])(implicit formats: Formats) {
  def serialize(value: Any): String = Printer.compact(render(toJValue(value)))
  def toJValue(value: Any): JValue = Extraction.decompose(value).capitalize
}

object JsonSerializer {
  def serialize[T](value: T)(implicit cm: ClassManifest[T], formats: Formats) = JsonSerializer(cm.erasure).serialize(value)
}