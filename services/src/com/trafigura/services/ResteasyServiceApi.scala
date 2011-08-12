package com.trafigura.services

import org.jboss.resteasy.spi.ResteasyProviderFactory
import starling.services.rpc.{JsonSerializerMessageBodyWriter, JsonDeserializerMessageBodyReader}
import org.jboss.resteasy.client.core.ClientErrorInterceptor
import org.jboss.resteasy.client.{ClientResponse, ProxyFactory}

import starling.utils.ImplicitConversions._
import starling.utils.ClosureUtil._
import xml._
import javax.xml.parsers.SAXParserFactory
import java.lang.StackTraceElement

case class ResteasyServiceApi(baseUri: String) extends ServiceApi {
  ResteasyServiceApi.registerProviderInstance

  override def create[T: ClassManifest] = ProxyFactory.create(classManifest[T].erasure, baseUri).asInstanceOf[T]
}

object ResteasyServiceApi {
  lazy val registerProviderInstance = {
    val providerFactory = ResteasyProviderFactory.getInstance
    providerFactory.registerProviderInstance(new JsonDeserializerMessageBodyReader)
    providerFactory.registerProviderInstance(new JsonSerializerMessageBodyWriter)
    providerFactory.addClientErrorInterceptor(RethrowDotNetServerErrorsClientErrorInterceptor)
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