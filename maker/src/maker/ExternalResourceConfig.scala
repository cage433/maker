package maker

import java.io.File
import maker.utils.FileUtils._

case class ExternalResourceConfig(configFile : File){
  private def extractMap(prefix : String) : Map[String, String] = {
    configFile.readLines.filter(_.startsWith(prefix)).map{
      line =>
        val List(key, value) = line.split(' ').filterNot(_.isEmpty).drop(1).toList
        key -> value
    }.toMap
  }
  def resourceVersions() = extractMap("version:")
  def resourceResolvers() = extractMap("resolver:")
}
 
