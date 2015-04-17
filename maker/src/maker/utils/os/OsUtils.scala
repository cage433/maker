package maker.utils.os
import scala.util.Properties

object OsUtils {
  def isLinux = Properties.osName.toLowerCase.contains("linux")
  def isOSX = Properties.osName.toLowerCase.contains("os x")
  def isUnix = isLinux || isOSX
}
