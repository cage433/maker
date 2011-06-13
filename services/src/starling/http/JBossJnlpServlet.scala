package starling.http

import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import java.net.InetAddress

class JBossJnlpServlet extends HttpServlet {

  val template = """<?xml version='1.0' encoding='UTF-8'?>
<jnlp href="webstart/launcher.jnlp" codebase="http://hostname:8080/starling/" spec="1.0+">
                <information>
                  <vendor>Trafigura</vendor>
                  <title>Starling: Oil</title>
                  <homepage href="http://starling"></homepage>
                  <description>Starling: Oil</description>
                  <icon href="icon.png"></icon>
                  <offline-allowed></offline-allowed>
                  <shortcut><menu submenu="Trafigura"></menu></shortcut>
                </information>
                <security>
                    <all-permissions></all-permissions>
                </security>
                <resources>
                  <!-- For people with USB monitors add:  java-vm-args="-Dsun.java2d.d3d=false"-->
                  <!-- with the options initial-heap-size='128m' max-heap-size='1024m'
                    Starling would not start on Scotts machine. There's just an error dialog:
                    'Could not create the java virtual machine' -->
                  <j2se java-vm-args="-Dsun.java2d.d3d=false" max-heap-size="812m" version="1.6+"></j2se>
                  <jar href="webstart/starling-booter-1.0.jar"></jar>
                </resources>
                <application-desc main-class="starling.booter.Booter">
                  <argument>http://hostname:8080/starling</argument>
                    <argument>JBossStarling</argument>
                </application-desc>
                  <update policy="always" check="always"></update>
              </jnlp>

  """

  private def makeJnlpText = template.replaceAll("hostname", InetAddress.getLocalHost().getHostName)

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) = {
    resp.setContentType("application/x-java-jnlp-file")
    resp.getWriter.print(makeJnlpText)
  }
}