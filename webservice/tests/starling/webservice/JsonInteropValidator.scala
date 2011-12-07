package starling.webservice

import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import org.apache.commons.io.filefilter.TrueFileFilter
import starling.utils.StringIO
import org.scalatest.matchers.ShouldMatchers

import scala.collection.JavaConversions._
import starling.utils.ClosureUtil._
import starling.utils.ImplicitConversions._
import java.lang.String
import java.io.File


object JsonInteropValidator extends ShouldMatchers {
  private implicit val formats = EDMFormats

  def main(args: Array[String]) {
    if (args.length < 2) {
      println("Usage: JsonInteropValidator <directory-containing-json> <package-containing-classes>")
    } else {
      validate(args(0), args(1))
    }
  }

  def validate(jsonDirectory: String, packageName: String) {
    val jsonFiles = filesInPackage(new File(jsonDirectory), packageName)
    val overlappingClasses = jsonFiles.toMapWithSomeKeys(_.matchingClass)

    overlappingClasses.foreach { case (overlappingClass, jsonFile) =>
      jsonFile.contents.foreach(json => assertRoundTrips(overlappingClass, json._1, json._2))
    }
  }

  private def filesInPackage(jsonDirectory: File, packageName: String): List[JSONFile] = {
    val directoryToSearch = new File(jsonDirectory, packageName.replaceAll("\\.", "/"))

    ApacheFileUtils.listFiles(directoryToSearch, TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE).toList.asInstanceOf[List[File]]
      .collect { case file if file.isFile && file.getName.endsWith(".json") => JSONFile(file, jsonDirectory) }
  }

  private def assertRoundTrips(clazz: Class[_], jsonText: String, unjoinedJson: String) {
    val input = decorate("Invalid json:\n%s" % unjoinedJson) {
      JsonDeserializer.pretty(jsonText)
    }

    val deserialized = decorate("Could not create %s from:\n%s" % (clazz.getName, unjoinedJson)) {
      JsonDeserializer.deserialize(input)
    }

    val roundTripped = decorate("Could not serialized %s" % deserialized) {
      JsonSerializer().serialize(deserialized)
    }

    input should be === roundTripped
  }

  case class JSONFile(file: File, jsonDirectory: File) {
    private val relativeFileName = file.getAbsolutePath.stripPrefix(jsonDirectory.getAbsolutePath + "/")
    private val className = relativeFileName.replace("/", ".").stripSuffix(".json")

    def contents: List[(String, String)] = StringIO.readJoinedLinesFromFileWithOriginal(file)
      .filterNot(pair => pair._1.isEmpty || pair._1.startsWith("#"))

    def matchingClass: Option[Class[Object]] = safely { Class.forName(className).asInstanceOf[Class[Object]] }.toOption
  }
}
