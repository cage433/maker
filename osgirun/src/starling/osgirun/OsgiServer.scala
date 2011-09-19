package starling.osgirun

import starling.osgirun.BundleConfig._
import java.io.{FileInputStream, File}
import java.util.{Dictionary, Properties}

object OsgiServer {
  def main(args:Array[String]) {

    def readProps = {
      val p = new Properties()
      List(new File("props.conf")).foreach { file =>
        if (file.exists) {
          p.load(new FileInputStream(file))
        }
      }
      dictionaryToMap[String,String](p)
    }
    OsgiInstance.startOrTrigger("osgi-server-cache", true, readProps _, ServerBundles)
  }

  def dictionaryToMap[K,V](dictionary:Dictionary[_,_]):Map[K,V] = {
    var map = Map[K,V]()
    val enumeration = dictionary.keys
    while (enumeration.hasMoreElements) {
      val key = enumeration.nextElement
      val value = dictionary.get(key)
      map = map.updated(key.asInstanceOf[K], value.asInstanceOf[V])
    }
    map
  }
}

object ServerBundles extends BundleDefinitions {

  def systemPackages = List(
    "sun.misc", "sun.reflect", "sun.reflect.generics.reflectiveObjects",
     "com.sun.jna", "com.sun.jna.platform.win32", "com.sun.jna.ptr", "com.sun.jna.win32" //not really needed by the server, maybe split auth?)
  )

  def bundles = {

    val workspace = new File(".")

    val ignoreJars = Set(
      "persistence-support-jar-2.14.jar",
      "dom4j-jar-1.6.1.jar",
      "commons-logging-jar-1.1.1.jar", "log4j-bundle-1.2.16.jar", "slf4j-api-jar-1.6.1.jar", "slf4j-log4j12-jar-1.6.1.jar",
      "jna.jar", "platform.jar",
      "testng-jar-5.8.jar", "scalatest-1.6.1.jar",
      "mockito-all-jar-1.8.2.jar",
      "derby-jar-10.5.3.0_1.jar",
      "hibernate-core-3.5.1-Final.jar",
      "h2-jar-1.2.131.jar", "hsqldb-jar-1.8.0.10.jar",
      "servlet-api-jar-2.5.jar",
      "scala-compiler-jar-2.9.1.jar",
      "scalacheck_2.9.1-jar-1.9.jar"
    )

    val libIgnoreJars = Set("scalatest-1.6.1.jar", "org.osgi.compendium-4.2.0.jar", "org.osgi.core-4.2.0.jar") ++ ignoreJars

    val sharedJars = FilesUtils.allFiles(
      //new File(workspace, ".ivy"),
      new File(workspace, "lib/scala"),
      new File(workspace, "lib/titan-model-jars"),
      new File(workspace, "lib/titan-other-jars")).
       filter(_.getName.endsWith(".jar")).
       filterNot(_.getName.contains("source")).
       filterNot(_.getName.contains("javadoc")).
       filterNot(f => f.getName.contains("spring-") && f.getName.contains("3.0.0.RELEASE")).filterNot(f=>ignoreJars.contains(f.getName))

    val copyJars = Set[String](
      //"spring-tx-jar-3.0.5.RELEASE.jar",
      //"spring-jdbc-jar-3.0.5.RELEASE.jar",
      //"spring-aop-3.0.5.RELEASE.jar",
      //"spring-core-jar-3.0.5.RELEASE.jar",
      //"spring-beans-jar-3.0.5.RELEASE.jar",
      //"spring-context-3.0.5.RELEASE.jar",
      //"spring-context-support-jar-3.0.5.RELEASE.jar"
    )

    val mergeJars = Map(
      ("titan-hub-persistence-model", "") -> List("scala-hub-support-2.14.jar", /*"persistence-support-2.3.jar",*/
        "scala-model-with-persistence.jar", "titan-core.jar", "titan-security.jar", "titan-utils.jar"),
      ("jeksparser-calculator", "") -> List("jeksparser.jar", "calculator.jar")
    )

    val (includes, excludes) = (
      List(
        "scala",
        "org.jboss.resteasy.logging.impl",
        //"com.sun.ws.rs.ext", not needed call RuntimeDelegate.setInstance(ResteasyProviderFactory.getInstance())
        "net.sourceforge.jtds.jdbc",
        "com.thoughtworks.xstream.converters.extended", "com.thoughtworks.xstream.converters.enums",
        "com.thoughtworks.xstream.converters.basic",
        "org.jboss.resteasy.client.core.marshallers"),
      List("com.sun.jna.*", "org.scalatest.*", "org.testng.*")
    )


    val manager = new Manager(
      workspace,
      sharedJars,
      copyJars,
      mergeJars,
      libIgnoreJars,
      Map(
        "osgimanager" -> BundleConfig(exportAll=true, dirs=List("osgimanager")),
        "manager" -> BundleConfig(exportAll=true, dirs=List("manager")),
        //"dbx" -> BundleConfig(exportAll=true, internalJars=List("bonecp-bundle-0.7.1.RELEASE.jar"), dirs=List("dbx")),
        "utils" -> BundleConfig(exportAll=true, dirs=List("utils")),
        "auth" -> BundleConfig(exportAll=true, dirs=List("auth")),
        "bouncyrmi" -> BundleConfig(exportAll=true, dirs=List("bouncyrmi")),
        "reports" -> BundleConfig(exportAll=true, dirs=List("reports")),
        "loopyxl" -> BundleConfig(exportAll=true, dirs=List("loopyxl")),
        "reports.internal" -> BundleConfig(exportAll=true, dirs=List("reports.internal")),
        "main" -> BundleConfig(true, List("joda-time-jar-1.6.jar", "bonecp-bundle-0.7.1.RELEASE.jar"), includes, excludes, None, List(
          "daterange", "titan", "browser.service", "fc2.api",
          "concurrent", "curves",
          "databases", "gui.api", "instrument",
          "maths", "pivot", "pivot.utils",
          "quantity", "services", "starling.api",
          "trade", "dbx"
        ))
      )
    )

    val osgiRuntimeBundles = FilesUtils.allFiles(new File("osgirun/bundles")).filter(_.getName.endsWith(".jar")).map { file =>
      new ExistingBundleDefinition(file)
    }

    val starlingBundlesAndJars = manager.definitions

    osgiRuntimeBundles ::: starlingBundlesAndJars
  }
}