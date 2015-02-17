package maker

import com.typesafe.config.{Config, ConfigFactory}
import scalaz.syntax.std.ToBooleanOps
import maker.utils.Int
import org.slf4j.LoggerFactory

trait MakerConfig {
  val config = ConfigFactory.load()

  implicit class RichConfig(config : Config) extends ToBooleanOps{
    val prefix = "com.github.cage433.maker."

    def proxy : Option[(String, Int)] = {
      config.getBoolean("maker.proxy.required").option(
        (config.getString("maker.proxy.host"), config.getInt("maker.proxy.port"))
      )
    }

    def scalaVersion : String = config.getString("maker.project.scala.version")

    def scalaLibraryResolver = config.getString("maker.project.scala.resolver")

    def projectScalaLibDirectory = config.getString("maker.project.scala.library-directory")
  }
}
