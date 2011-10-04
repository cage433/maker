package starling.osgirun

import java.util.Properties
import java.io.{FileInputStream, File}
import java.io.File
import java.net.URL

object OsgiGui {

  def main(args:Array[String]) {
    println("Args: " + args.toList)
    System.setProperty("org.osgi.service.http.port", "7777")
    start(args(0), args(1).toInt)
    if (args.length == 3) {
      val st = args(2)
      val index = st.indexOf("://")
      val s = st.substring(index + 3)
      val url = new URL("http://localhost:7777/" + s)
      val stream = url.openStream()
      stream.close()
    }
  }

  def start(host:String, port:Int) {
    OsgiInstance.startOrTrigger("osgi-gui-cache", false, ()=>Map("ServerRmiHost" -> host, "serverRmiPort"->port.toString), GuiBundles)
  }
}

object DevOsgiGui {
  def main(args:Array[String]) {
    val p = new Properties()
    List(new File("props.conf"), new File("generated.props.conf")).foreach { file =>
      if (file.exists) {
        p.load(new FileInputStream(file))
      }
    }
    val port = p.get("RmiPort").asInstanceOf[String]
    OsgiGui.start("localhost", port.toInt)
  }
}
class OsgiGui //the object OsgiGui is not built without this line

object GuiBundles extends BundleDefinitions {

  def systemPackages = List("sun.misc", "sun.reflect", "com.sun.jna", "com.sun.jna.ptr", "com.sun.jna.win32", "com.sun.management")

  def bundles = {


    val sharedJars = List(
      new File("browser/lib/jxlayer-4.0.jar"),
      new File("browser/lib/looks-2.3.1.jar"),
      new File("browser/lib/miglayout-4.0-swing.jar"),
      new File("browser/lib/org.eclipse.mylyn.wikitext.core_1.4.0.I20100805-0500-e3x.jar"),
      new File("browser/lib/org.eclipse.mylyn.wikitext.textile.core_1.4.0.I20100805-0500-e3x.jar"),
      new File("browser/lib/swingx-core-1.6.2.jar"),
      new File("browser/lib/timingframework-1.0.jar"),
      new File("utils/lib_managed/google-collections-jar-1.0.jar"),
      new File("utils/lib_managed/xstream-jar-1.3.1.jar"),
      new File("utils/lib_managed/cglib-nodep-jar-2.2.jar"),
      new File("lib/scala/lib_managed/scala-library-jar-2.9.1.jar"),
      new File("lib/scala/lib_managed/scala-swing-jar-2.9.1.jar"),
      new File("osgirun/bundles/org.apache.felix.configadmin-1.2.8.jar"),


      //GUI
      new File("utils/lib_managed/commons-io-jar-1.3.2.jar"),
      new File("osgirun/bundles/pax-logging-api-1.6.3.jar"),
      new File("osgirun/bundles/pax-logging-service-1.6.3.jar"),
      new File("bouncyrmi/lib_managed/netty-bundle-3.2.5.Final.jar"),
      new File("gui/lib_managed/jfreechart-jar-1.0.0.jar"),
      new File("gui/lib_managed/jcommon-jar-1.0.0.jar"),
      new File("utils/lib_managed/joda-time-jar-1.6.jar")
    )

    sharedJars.foreach { file => {
      if (!file.exists) println(file + " does not exist")
    }}

    val includes = List("com.thoughtworks.xstream.converters.extended", "com.thoughtworks.xstream.converters.enums",
        "com.thoughtworks.xstream.converters.basic", "org.joda.time.base", "org.joda.time.chrono", "org.jboss.netty.channel", "net.sf.cglib.reflect")
    val excludes = List("cern.*", "com.rabbitmq.*", "net.spy.memcached.*", "org.apache.commons.codec.digest", "org.testng", "sjson.json.*")

    val bundles = Map(
        "browser" -> BundleConfig(exportAll=true, dirs = List("browser")),
        "browser.service" -> BundleConfig(exportAll=true, dirs = List("browser.service")),
        "osgimanager" -> BundleConfig(exportAll=true, dirs=List("osgimanager")),
        "manager" -> BundleConfig(exportAll=true, dirs = List("manager")),
        "gui" -> BundleConfig(true, Nil, includes, excludes, Some("gui"),
          List("daterange", "quantity", "utils", "auth", "bouncyrmi", "gui", "gui.api", "reports", "pivot", "pivot.utils", "fc2.api")))

    val manager = new Manager(
      new File("."),
      sharedJars,
      Set("org.apache.felix.configadmin-1.2.8.jar", "pax-logging-service-1.6.3.jar", "pax-logging-api-1.6.3.jar"),
      Map(),
      Set("scalatest-1.6.1.jar",
          "dispatch-json_2.8.0-0.7.4.jar",
          "memcached-2.5.jar",
          "log4j-bundle-1.2.16.jar",
          "commons-logging-jar-1.1.1.jar",
          "mockito-all-jar-1.8.2.jar",
          "sjson_2.8.0-jar-0.8.jar",
          "testng-jar-5.8.jar",
          "org.osgi.compendium-4.2.0.jar",
          "org.osgi.core-4.2.0.jar"),
      bundles
    )

    manager.definitions ::: List(
      new ExistingBundleDefinition(new File("osgirun/bundles/pax-web-extender-whiteboard-1.1.1.jar")),
      new ExistingBundleDefinition(new File("osgirun/bundles/pax-web-jetty-bundle-1.1.1.jar"))
    )
  }
}