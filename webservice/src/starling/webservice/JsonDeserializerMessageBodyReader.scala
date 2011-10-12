package starling.webservice

import java.lang.reflect.Type
import javax.ws.rs.core.{MultivaluedMap, MediaType}
import javax.ws.rs.Consumes
import javax.ws.rs.ext.{MessageBodyReader, Provider}
import java.lang.annotation.Annotation
import java.lang.{String, Class}
import java.io.InputStream
import org.jboss.resteasy.plugins.providers.ProviderHelper
import collection.immutable.Map


@Provider
@Consumes(Array("application/json"))
class JsonDeserializerMessageBodyReader extends MessageBodyReader[Any] {
  private implicit val formats = EDMFormats
  private val deserializers: Map[Class[_], (String) => Any] = Map(
    classOf[Boolean] → java.lang.Boolean.parseBoolean _,
    classOf[Int]     → ((string:String) => java.lang.Integer.parseInt(string))
  )

  def readFrom(clazz : Class[Any], genericType: Type, annotations: Array[Annotation], mediaType: MediaType,
               httpHeaders: MultivaluedMap[String, String], entityStream: InputStream) = {

    val deserialize = deserializers.getOrElse(clazz, JsonDeserializer.deserialize _)

    deserialize(ProviderHelper.readString(entityStream, MediaType.APPLICATION_JSON_TYPE))
  }

  def isReadable(clazz : Class[_], genericType: Type, annotations: Array[Annotation], mediaType: MediaType) = true
}