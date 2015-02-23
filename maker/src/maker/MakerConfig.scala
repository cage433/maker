package maker

import com.typesafe.config.{Config, ConfigFactory}
import scalaz.syntax.std.ToBooleanOps
import maker.utils.Int
import org.slf4j.LoggerFactory
import maker.utils.FileUtils._
import scala.collection.JavaConversions._
import java.io.File

trait MakerConfig {
  val config = ConfigFactory.load()

  implicit class RichConfig(config : Config) extends ToBooleanOps{
    val prefix = "com.github.cage433.maker."

    def proxy : Option[(String, Int)] = {
      config.getBoolean("maker.http.proxy.required").option(
        (config.getString("maker.http.proxy.host"), config.getInt("maker.http.proxy.port"))
      )
    }

    def scalaVersion : ScalaVersion = ScalaVersion(config.getString("maker.project.scala.version"))

    def scalaLibraryResolver = config.getString("maker.project.scala.resolver")

    def projectScalaLibDirectory : File = {
      val dir = file(config.getString("maker.project.scala.library-directory"))
      dir.mkdirs
      dir
    }

    def resourceCache = mkdirs(file(config.getString("maker.resource-cache")))

    def resolvers = config.getStringList("maker.http.resolvers")

    def httpHeaders = config.getStringList("maker.http.headers").map{
      case header =>
        val Array(field, value) = header.split(":")
        (field, value)
    }
      
  }
}
