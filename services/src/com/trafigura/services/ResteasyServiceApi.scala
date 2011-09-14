package com.trafigura.services

import starling.services.rpc.{JsonSerializerMessageBodyWriter, JsonDeserializerMessageBodyReader}
import org.jboss.resteasy.client.core.ClientErrorInterceptor
import org.jboss.resteasy.client.{ClientResponse, ProxyFactory}

import starling.utils.ImplicitConversions._
import starling.utils.ClosureUtil._
import xml._
import javax.xml.parsers.SAXParserFactory
import java.lang.StackTraceElement
import org.jboss.resteasy.spi.ResteasyProviderFactory
import javax.ws.rs.ext.RuntimeDelegate
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import javax.ws.rs.ext.ExceptionMapper
import scala.collection.JavaConversions._
import org.jboss.resteasy.spi.{BadRequestException, StringConverter, ResteasyProviderFactory}
import javax.ws.rs.core.Response.Status
import javax.ws.rs.core.{MediaType, Response}
import java.lang.reflect.InvocationTargetException

case class ResteasyServiceApi(baseUri: String) extends ServiceApi {
  ResteasyServiceApi.registerProviderInstance

  override def create[T: ClassManifest] = ProxyFactory.create(classManifest[T].erasure, baseUri).asInstanceOf[T]
}

object ResteasyServiceApi {

  {
     //RuntimeDelegate.setInstance(ResteasyProviderFactory.getInstance())
  }

  lazy val registerProviderInstance = {
    val providerFactory = ResteasyProviderFactory.getInstance
    providerFactory.registerProviderInstance(new JsonDeserializerMessageBodyReader)
    providerFactory.registerProviderInstance(new JsonSerializerMessageBodyWriter)
    providerFactory.addClientErrorInterceptor(RethrowDotNetServerErrorsClientErrorInterceptor)
    providerFactory.registerProviderInstance(LocalDateConverter)
    providerFactory.addExceptionMapper(BadRequestExceptionMapper)
  }

  class BaseExceptionMapper[T <: Throwable](status: Status = Status.BAD_REQUEST) extends ExceptionMapper[T] {
    def toResponse(throwable: T) = response(getCause(throwable.getCause))

    private def response(throwable: Throwable): Response = {
      Response.status(status).entity(
        <html>
          <head><title>{throwable.getMessage}</title></head>
          <body><div>{throwable.stackTraceAsString}</div></body>
        </html>.toString()).`type`(MediaType.TEXT_HTML_TYPE).build
    }

    def getCause(throwable: Throwable): Throwable = {
      if (throwable.isInstanceOf[InvocationTargetException]) getCause(throwable.getCause) else throwable
    }
   }

  object BadRequestExceptionMapper extends BaseExceptionMapper[BadRequestException] with ExceptionMapper[BadRequestException]

  object LocalDateConverter extends StringConverter[LocalDate] {
    def fromString(text: String) = DateTimeFormat.forPattern("yyyy'-'MM'-'dd").parseDateTime(text).toLocalDate
    def toString(value: LocalDate) = value.toString
  }

  object RethrowDotNetServerErrorsClientErrorInterceptor extends ClientErrorInterceptor {
    def handle(response: ClientResponse[_]) {
      val responseString = response.getEntity(classOf[String])

      val exception = decorate("Unable to extract exception in\n" + responseString) {
        extractException(responseString)
      }

      throw exception
    }

    private def extractException(responseString: String): RuntimeException = {
      val xml = Utility.trim(XML.loadXML(Source.fromString(responseString.dropWhile(_ != '<')), parser))
      val bodyContent = xml \\ "body" \\ "div" \ "_"

      val message = bodyContent.find({ case <p>{paragraph}</p> => paragraph.text.contains("The exception message is") })
        .map(_.text).getOrElse(bodyContent.toString)

      val stackTrace = bodyContent.find({ case <p>{paragraph}</p> => paragraph.text.contains(" at") })
        .map(_.text).getOrElse("")

      val ClassMethod = """(.*)\.(.*)\(.*""".r
      val ClassMethodFileLine = """(.*)\.(.*)\(.*\) in (.*):line (\d+)""".r

      val stackTraceElements = stackTrace.split("at ").map(_.trim).filterNot(_ == "").flatMap(_.partialMatch {
        case ClassMethodFileLine(clazz, method, file, line) => new StackTraceElement(clazz, method, file, line.toInt)
        case ClassMethod(clazz, method) => new StackTraceElement(clazz, method, null, -1)
      })

      val cause = new RuntimeException(message)
      cause.setStackTrace(stackTraceElements)

      new RuntimeException(cause)
    }

    private def parser: SAXParser = {
      val f = SAXParserFactory.newInstance()
      f.setNamespaceAware(false)
      f.setValidating(false)
      f.setXIncludeAware(false)
      // Ignore the DTD declaration (stop the parser from downloading the DTD for html !)
      f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
      f.setFeature("http://xml.org/sax/features/validation", false);

      f.newSAXParser()
    }
  }
}
