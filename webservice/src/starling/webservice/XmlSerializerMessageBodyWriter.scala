package starling.webservice

import java.lang.reflect.Type
import java.lang.annotation.Annotation
import javax.ws.rs.core.{MultivaluedMap, MediaType}
import java.lang.{String, Class}
import java.io.OutputStream
import javax.ws.rs.ext.{Provider, MessageBodyWriter}
import javax.ws.rs.Produces
import xml.{Elem, Node}

@Provider
@Produces(Array("application/xml"))
class XmlSerializerMessageBodyWriter extends MessageBodyWriter[Any] {
  import RichJValue._

  private implicit val formats = EDMFormats

  def isWriteable(clazz : Class[_], genericType: Type, annotations: Array[Annotation], mediaType: MediaType) = true

  def getSize(value: Any, clazz : Class[_], genericType: Type, annotations: Array[Annotation], mediaType: MediaType) =
    serialize(clazz, value).toString.getBytes.size

  def writeTo(value: Any, clazz : Class[_], genericType: Type, annotations: Array[Annotation], mediaType: MediaType,
              httpHeaders: MultivaluedMap[String, AnyRef], entityStream: OutputStream) {
    entityStream.write(serialize(clazz, value).toString.getBytes)
  }

  def serialize(clazz: Class[_], value: Any): Elem = if (clazz.isPrimitive || clazz == classOf[String]) {
    element(clazz.getSimpleName, value)
  } else {
    JsonSerializer(clazz).toJValue(value).toXml
  }

  private def element(name: String, contents: Any): Elem = <tmp>{contents}</tmp>.copy(label = name)
}