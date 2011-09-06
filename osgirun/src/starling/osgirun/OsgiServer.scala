package starling.osgirun

import java.io.File

object OsgiServer {
  def main(args:Array[String]) {
    OsgiInstance.startOrTrigger("osgi-server-cache", ServerBundles)
  }

}

object ServerBundles extends BundleDefinitions {

  def systemPackages = List("sun.misc", "sun.reflect", "sun.reflect.generics.reflectiveObjects")

  def bundles = {

    val workspace = new File(".")

    val ignoreJars = Set(
      //"joda-time-1.6.jar", //in main at the moment
      "main_2.8.1-0.10.0.jar", "persist_2.8.1-0.10.0.jar", "tasks_2.8.1-0.10.0.jar", "sbt_2.8.1-0.10.0.jar",
      "dom4j-1.6.1.jar",
      "scala-compiler.jar",
      "commons-logging-1.1.1.jar", "log4j-1.2.16.jar", "slf4j-api-1.6.1.jar", "slf4j-log4j12-1.6.1.jar",
      "testng-5.8-jdk15.jar", "jna-3.0.9.jar", "platform.jar",
      "junit-3.8.1.jar", "junit-3.8.jar", "scalatest_2.9.0-1.4.1.jar",
      "mockito-all-1.8.2.jar",
      "derby-10.5.3.0_1.jar",
      "jsch-0.1.31.jar",
      "hibernate-annotations-3.4.0.GA.jar", "hibernate-core-3.3.2.GA.jar",
      "ivy-2.2.0.jar", "ivy_2.8.1-0.10.0.jar",
      //"servlet-api-2.5-20081211.jar",
      "h2-1.2.131.jar", "hsqldb-1.8.0.10.jar",
      "servlet-api-2.5.jar",
      "scala-compiler-src.jar", "scala-swing-src.jar", "scala-library-src.jar", "scala-compiler-2.8.1.jar", "scala-compiler-2.9.0-1.jar",
      "scalacheck-2.9.0-1-1.9.jar",
      "org.osgi.compendium-4.2.0.jar", "org.osgi.core-4.2.0.jar"
    )

    val libIgnoreJars = Set("scalatest-1.6.1.jar")

    val sharedJars = FilesUtils.allFiles(
      new File(workspace, ".ivy"),
      new File(workspace, "lib/scala"),
      new File(workspace, "lib/titan-model-jars"),
      new File(workspace, "lib/titan-other-jars")).
       filter(_.getName.endsWith(".jar")).
       filterNot(_.getName.endsWith("sources.jar")).
       filterNot(_.getPath.contains("org.scala-tools.sbt")).
       filterNot(f => f.getName.contains("spring-") && f.getName.contains("3.0.0.RELEASE")).filterNot(f=>ignoreJars.contains(f.getName))

    val copyJars = Set(
      "spring-tx-3.0.5.RELEASE.jar", "spring-jdbc-3.0.5.RELEASE.jar",
      "spring-aop-3.0.5.RELEASE.jar", "spring-core-3.0.5.RELEASE.jar",
      "spring-beans-3.0.5.RELEASE.jar", "spring-context-3.0.5.RELEASE.jar",
      "spring-context-support-3.0.5.RELEASE.jar"
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
        //"com.sun.ws.rs.ext", not needed call RuntimeDelegate.setInstance(new ResteasyProviderFactory());
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
        "osgimanager" -> BundleConfig(dirs=List("osgimanager")),
        "manager" -> BundleConfig(exportAll=true, dirs=List("manager")),
        "dbx" -> BundleConfig(exportAll=true, internalJars=List("bonecp-0.7.1.RELEASE.jar"), dirs=List("dbx")),
        //"utils" -> BundleConfig(exportAll=true, dirs=List("utils")),
        "main" -> BundleConfig(false, Nil, includes, excludes, List(
          "daterange", "titan", "browser.service", "fc2.api",
          "auth", "bouncyrmi", "concurrent", "curves",
          "databases", "gui.api", "instrument", "loopyxl",
          "maths", "pivot", "pivot.utils",
          "quantity", "services", "starling.api",
          "trade", "utils", "var"
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