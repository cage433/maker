package maker.utils.os
import maker.MakerProps
import scala.util.Properties

object OsUtils {
  def isLinux = Properties.osName.toLowerCase.contains("linux")
  def isOSX = Properties.osName.toLowerCase.contains("os x")
  def isUnix = isLinux || isOSX
  def isPortUsed(port : Int) = {
    List("tcp", "udp").exists{ t => 
      Command("fuser", port + "/" + t).withNoOutput.exec == 0
    }
  }
}
