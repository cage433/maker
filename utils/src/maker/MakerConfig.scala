package maker

import com.typesafe.config.{Config, ConfigFactory}
import scalaz.syntax.std.ToBooleanOps
import maker.utils.Int
import org.slf4j.LoggerFactory

trait MakerConfig {
  val config = ConfigFactory.load()

  implicit class RichConfig(config : Config) extends ToBooleanOps{
    val prefix = "com.github.cage433.maker."

    private def addPrefix(path : String) = s"${prefix}${path}"
    private def getInt(path : String) = config.getInt(addPrefix(path))
    private def getString(path : String) = config.getString(addPrefix(path))
    private def getBoolean(path : String) = config.getBoolean(addPrefix(path))

    def proxy : Option[(String, Int)] = {
      getBoolean("use.proxy").option(
        (getString("proxy.host"), getInt("proxy.port"))
      )
    }

    def scalaVersion : String = getString("scala.version")
  }
}
