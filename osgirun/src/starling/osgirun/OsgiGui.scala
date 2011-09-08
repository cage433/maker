package starling.osgirun

import java.io.File
import java.net.URL

object OsgiGui {

  def main(args:Array[String]) {
    println("Args: " + args.toList)
    System.setProperty("org.osgi.service.http.port", "7777")
    OsgiInstance.startOrTrigger("osgi-gui-cache", GuiBundles)
    if (args.length == 1) {
      val st = args(0)
      val index = st.indexOf("://")
      val s = st.substring(index + 3)
      val url = new URL("http://localhost:7777/" + s)
      val stream = url.openStream()
      stream.close()
    }
  }

}
class OsgiGui //the object OsgiGui is not built without this line

object GuiBundles extends BundleDefinitions {

  def systemPackages = List("sun.misc", "sun.reflect", "com.sun.jna", "com.sun.jna.ptr", "com.sun.jna.win32", "com.sun.management")

  def bundles = {

    val browserLib = new File("browser/lib")
    val sharedJars = List("jxlayer-4.0.jar", "looks-2.3.1.jar", "Miglayout-3-7-3-1-nick.jar",
      "org.eclipse.mylyn.wikitext.core_1.4.0.I20100805-0500-e3x.jar",
      "org.eclipse.mylyn.wikitext.textile.core_1.4.0.I20100805-0500-e3x.jar",
      "swingx-core-1.6.2.jar", "timingframework-1.0.jar"
    ).map(name => new File(browserLib, name)) ::: List(
      new File(".ivy/cache/com.google.collections/google-collections/jars/google-collections-1.0.jar"),
      new File(".ivy/cache/com.thoughtworks.xstream/xstream/jars/xstream-1.3.1.jar"),
      new File(".ivy/cache/cglib/cglib-nodep/jars/cglib-nodep-2.2.jar"),
      new File("lib/scala/scala-2.9.0.1.final/lib/scala-library.jar"),
      new File("lib/scala/scala-2.9.0.1.final/lib/scala-swing.jar"),
      new File("osgirun/bundles/org.apache.felix.configadmin-1.2.8.jar"),


      //GUI
      new File(".ivy/cache/commons-io/commons-io/jars/commons-io-1.3.2.jar"),
      new File("osgirun/bundles/pax-logging-api-1.6.3.jar"),
      new File("osgirun/bundles/pax-logging-service-1.6.3.jar"),
      new File(".ivy/cache/org.jboss.netty/netty/jars/netty-3.2.5.Final.jar"),
      new File(".ivy/cache/jfree/jfreechart/jars/jfreechart-1.0.0.jar"),
      new File(".ivy/cache/jfree/jcommon/jars/jcommon-1.0.0.jar"),
      new File(".ivy/cache/joda-time/joda-time/jars/joda-time-1.6.jar")

    )

    val includes = List("com.thoughtworks.xstream.converters.extended", "com.thoughtworks.xstream.converters.enums",
        "com.thoughtworks.xstream.converters.basic", "org.joda.time.base", "org.joda.time.chrono", "org.jboss.netty.channel", "net.sf.cglib.reflect")
    val excludes = List("cern.*", "com.rabbitmq.*", "net.spy.memcached.*", "org.apache.commons.codec.digest", "org.testng", "sjson.json.*")

    val bundles = Map(
        "browser" -> BundleConfig(exportAll = true, dirs = List("browser")),
        "browser.service" -> BundleConfig(exportAll = true, dirs = List("browser.service")),
        "osgimanager" -> BundleConfig(dirs=List("osgimanager")),
        "manager" -> BundleConfig(exportAll=true, dirs = List("manager")),
        "gui" -> BundleConfig(false, Nil, includes, excludes,
          List("daterange", "quantity", "utils", "auth", "bouncyrmi", "gui", "gui.api", "pivot", "pivot.utils", "fc2.api")))

    val manager = new Manager(
      new File("."),
      sharedJars,
      Set("org.apache.felix.configadmin-1.2.8.jar", "pax-logging-service-1.6.3.jar", "pax-logging-api-1.6.3.jar"),
      Map(),
      Set("scalatest-1.6.1.jar", "dispatch-json_2.8.0-0.7.4.jar", "memcached-2.5.jar", "org.osgi.compendium-4.2.0.jar", "org.osgi.core-4.2.0.jar"),
      bundles
    )

    manager.definitions ::: List(
      new ExistingBundleDefinition(new File("osgirun/bundles/pax-web-extender-whiteboard-1.1.1.jar")),
      new ExistingBundleDefinition(new File("osgirun/bundles/pax-web-jetty-bundle-1.1.1.jar"))
    )
  }
}