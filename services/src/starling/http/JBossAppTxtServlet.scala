package starling.http

import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import java.net.InetAddress

class JBossAppTxtServlet extends HttpServlet{
  import JBossAppTxtServlet._
  import JBossStarlingServlet._

  val template = """starling.gui.Launcher hostname RmiPort STARLING-TEST/dave-linux
backport-util-concurrent-3.1.jar compilation-time
cglib-nodep-2.2.jar compilation-time
commons-codec-1.4.jar compilation-time
commons-io-1.3.2.jar compilation-time
commons-lang-2.3.jar compilation-time
commons-logging-1.1.1.jar compilation-time
commons-pool-1.3.jar compilation-time
dispatch-json_2.8.0-0.7.4.jar compilation-time
google-collect-1.0.jar compilation-time
jcommon-1.0.0.jar compilation-time
jfreechart-1.0.0.jar compilation-time
jna.jar compilation-time
joda-time-1.6.jar compilation-time
jsr107cache-1.0.jar compilation-time
jxlayer-4.0.jar compilation-time
log4j-1.2.14.jar compilation-time
looks-2.3.1.jar compilation-time
memcached-2.5.jar compilation-time
Miglayout-3-7-3-1-nick.jar compilation-time
mockito-all-1.8.2.jar compilation-time
netty-3.2.3.Final.jar compilation-time
org.eclipse.mylyn.wikitext.core_1.4.0.I20100805-0500-e3x.jar compilation-time
org.eclipse.mylyn.wikitext.textile.core_1.4.0.I20100805-0500-e3x.jar compilation-time
platform.jar compilation-time
scala-library.jar compilation-time
scala-swing.jar compilation-time
sjson-0.8-2.8.0.jar compilation-time
starling-auth-1.0.jar compilation-time
starling-bouncy-rmi-1.0.jar compilation-time
starling-daterange-1.0.jar compilation-time
starling-gui.api-1.0.jar compilation-time
starling-gui-1.0.jar compilation-time
starling-pivot-1.0.jar compilation-time
starling-quantity-1.0.jar compilation-time
starling-utils-1.0.jar compilation-time
swingx-core-1.6.2.jar compilation-time
timingframework-1.0.jar compilation-time
"""


  private def makeAppTxtString : String = {
    val hostname = InetAddress.getLocalHost().getHostName
    template.replace("hostname", hostname).replaceAll("compilation-time", launchTime).replace("RmiPort", props.RmiPort().toString)
  }

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) = {
    resp.setContentType("text/plain")
    resp.getWriter.print(makeAppTxtString)
  }
}

object JBossAppTxtServlet{
  // TODO - build time would be better
  val launchTime = System.currentTimeMillis.toString
}
