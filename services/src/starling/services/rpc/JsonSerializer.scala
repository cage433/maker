package starling.services.rpc

import net.liftweb.json._

import RichJValue._
import java.lang.reflect.Type
import java.lang.annotation.Annotation
import javax.ws.rs.core.{MultivaluedMap, MediaType}
import java.lang.{String, Class}
import java.io.OutputStream
import javax.ws.rs.ext.{Provider, MessageBodyWriter}
import javax.ws.rs.Produces
import org.jboss.resteasy.annotations.interception.ServerInterceptor
import org.jboss.resteasy.spi.interception.{MessageBodyWriterContext, MessageBodyWriterInterceptor}

case class JsonSerializer(clazz: Class[_])(implicit formats: Formats) {
  def serialize(value: Any): String = Printer.compact(render(toJValue(value)))
  def toJValue(value: Any): JValue = Extraction.decompose(value).capitalize
}

object JsonSerializer {
  def serialize[T](value: T)(implicit cm: ClassManifest[T], formats: Formats) = JsonSerializer(cm.erasure).serialize(value)
}

@Provider
@Produces(Array("application/json"))
class JsonSerializerMessageBodyWriter extends MessageBodyWriter[Any] {
  implicit val formats = EDMFormats

  def writeTo(value: Any, clazz : Class[_], genericType: Type, annotations: Array[Annotation], mediaType: MediaType,
              httpHeaders: MultivaluedMap[String, AnyRef], entityStream: OutputStream) {

    entityStream.write(JsonSerializer(clazz).serialize(value).getBytes)
  }

  def getSize(value: Any, clazz : Class[_], genericType: Type, annotations: Array[Annotation], mediaType: MediaType) = {
    JsonSerializer(clazz).serialize(value).getBytes.length
  }

  def isWriteable(clazz : Class[_], genericType: Type, annotations: Array[Annotation], mediaType: MediaType) = {
    true
  }
}
