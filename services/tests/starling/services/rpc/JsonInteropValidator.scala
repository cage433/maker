package starling.services.rpc

import java.io.File
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter
import starling.utils.StringIO
import org.scalatest.matchers.ShouldMatchers

import scala.collection.JavaConversions._
import starling.utils.ClosureUtil._
import starling.utils.ImplicitConversions._


object JsonInteropValidator extends ShouldMatchers {
  private implicit val formats = EDMFormats

  def main(args: Array[String]) {
    if (args.length < 2) {
      println("Usage: JsonInteropValidator <directory-containing-json> <package-containing-classes>")
    } else {
      validate(args(0), args(1))
    }
  }

  def validate(directoryContainingJSON: String, packageName: String) {
    val jsonFiles = filesInPackage(directoryContainingJSON, packageName)
    val overlappingClasses = jsonFiles.toMapWithSomeKeys(_.matchingClass)

    overlappingClasses.foreach { case (overlappingClass, jsonFile) =>
      jsonFile.contents.foreach(json => assertRoundTrips(overlappingClass, json._1, json._2))
    }
  }

  private def filesInPackage(directoryContainingJSON: String, packageName: String): List[JSONFile] = {
    val directoryToSearch = new File(directoryContainingJSON + "/" + packageName.replaceAll(".", "/"))

    FileUtils.listFiles(directoryToSearch, TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE).toList.asInstanceOf[List[File]]
      .collect { case file if file.isFile && file.getName.endsWith(".json") => JSONFile(file, directoryToSearch) }
  }

  private def assertRoundTrips(clazz: Class[_], jsonText: String, unjoinedJson: String) {
    val input = decorate("Invalid json:\n%s" % unjoinedJson) {
      JsonDeserializer.pretty(jsonText)
    }

    val deserialized = decorate("Could not create %s from:\n%s" % (clazz.getName, unjoinedJson)) {
      JsonDeserializer.deserialize(input)
    }

    val roundTripped = decorate("Could not serialized %s" % deserialized) {
      JsonSerializer(clazz).serialize(deserialized)
    }

    input should be === roundTripped
  }

  case class JSONFile(file: File, containingDirectory: File) {
    private val relativeFileName = file.getAbsolutePath.stripPrefix(containingDirectory.getAbsolutePath + "/")
    private val className = relativeFileName.replace("/", ".").stripSuffix(".json")

    def contents: List[(String, String)] = StringIO.readJoinedLinesFromFileWithOriginal(file)
      .filterNot(pair => pair._1.isEmpty || pair._1.startsWith("#"))

    def matchingClass: Option[Class[Object]] = safely { Class.forName(className).asInstanceOf[Class[Object]] }.toOption
  }
}