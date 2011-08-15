package com.trafigura.services

import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter
import javax.ws.rs.core._
import java.io.File
import collection.mutable.ListBuffer
import com.trafigura.services.meta.WebService
import starling.utils.StringIO

import scalaz._
import Scalaz._
import scala.collection.JavaConversions._
import starling.utils.ImplicitConversions._


object DocumentationService extends DocumentationServiceApi {
  private val serviceClasses = new ListBuffer[Class[_]] += getClass
  private val files: Map[String, File] = FileUtils.listFiles(new File("services/resources/com/trafigura/services/meta"),
    TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE).toList.asInstanceOf[List[File]].toMapWithKeys(_.getName)

  private val mediaTypes = Map(
    ".html" → MediaType.TEXT_HTML_TYPE,
    ".htm"  → MediaType.TEXT_HTML_TYPE,
    ".js"   → MediaType.valueOf("text/javascript"),
    ".css"  → MediaType.valueOf("text/css"),
    ".gif"  → MediaType.valueOf("image/gif")
  ).withDefault(_ => MediaType.valueOf("text/plain"))

  def registerInstances(instances: AnyRef*) = registerClasses(instances.map(_.getClass) : _*)
  def registerClasses(classes: Class[_]*) = serviceClasses ++= classes
  def services: Map[String, WebService] = serviceClasses.map(WebService.fromClass(_)).toMapWithKeys(_.uri.stripPrefix("/"))
  def file(filename: String): Response = bytes(filename).fold(Response.ok(_, mediaType(filename)), Response.noContent()).build
  def metaData(serviceUri: String): WebService = services.get(serviceUri.stripPrefix("/")).get
  def allMetaData: List[WebService] = services.values.toList

  def forms(serviceUri: String): String =
<html>
  <head>
    <title>{serviceUri}</title>
    <link rel='stylesheet' type='text/css' href={prefix("Doc/Files/jsonVisualization.css")} />
	  <script type='text/javascript' src={prefix("Doc/Files/yuiloader-dom-event.js")}></script>
	  <script type='text/javascript' src={prefix("Doc/Files/json-min.js")}></script>
    <script type='text/javascript' src={prefix("Doc/Files/jsonVisualization.js")}></script>
    <script type='text/javascript' src={prefix("Doc/Files/jquery-1.6.2.min.js")}></script>
    <script type='text/javascript' src={prefix("Doc/Files/jquery.tmpl.js")}></script>
    <script type='text/javascript' src={prefix("Doc/Files/auto-grow.js")}></script>
    <script type='text/javascript' src={prefix("Doc/Files/displayJson.js")}></script>
  </head>
  <body href="/Doc/Forms">
    <label><input type='radio' name='renderingStyle' id='html' value='json2HTML' checked='checked' />HTML</label>
    <label><input type='radio' name='renderingStyle' id='json' value='json2JSON' />JSON</label>
    <label><input type='radio' name='renderingStyle' id='scala' value='showScala'/>Scala</label>
    <label><input type='radio' name='renderingStyle' id='cs'    value='showCS' />CS</label>

    jquery-templates

    <div id='service' href={serviceUri}>
        <div class='json html rendering'></div>
        <div class='scala rendering'></div>
        <div class='cs rendering'></div>
    </div>
  </body>
</html>.toString.replace("jquery-templates", StringIO.readStringFromFile(files("jquery-templates.html")))

  private def mediaType(filename: String): MediaType = mediaTypes(filename.substring(filename.lastIndexOf(".")))
  private def prefix(input: String) = "/RPC/" + input
  private def bytes(filename: String) = if (!files.contains(filename)) None else Some(StringIO.readBytesFromFile(files(filename)))
}