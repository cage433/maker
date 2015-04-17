import scala.util.Properties

Properties.setProp("scala.usejavacp", "false")
Properties.setProp("log4j.ignoreTCL", "true")



import scala.xml.NodeSeq
import maker.task.FailingTests._
import maker.task.compile._
import maker.project._
import maker.utils.FileUtils._

object ResourcePimps extends maker.ResourcePimps
import ResourcePimps._

val extraPomInfo : List[NodeSeq] = {
  val devNodes = List("Alex McGuire", "Louis Botterill", "Sam Halliday").map{name => <developer><name>{name}</name></developer>}
  List(
    <name>Maker</name>
    <description>A scala build tool</description>
    <developers>{devNodes}</developers>
    <licenses>
      <license>
        <name>BSD 2 clause</name>
      </license>
    </licenses>
    <url>https:github.com/cage433/maker</url> 
    <scm>
      <url>https://github.com/cage433/maker</url> 
    </scm>
  )
}

lazy val testReporter = new Module(
  root = file("test-reporter").asAbsoluteFile, 
  name = "maker-test-reporter"
) with ClassicLayout {
  override def extraProjectPomInfo = extraPomInfo
  override def organization = Some("com.github.cage433")
  override def resources() = {
    Vector(
      "org.scalatest" % "scalatest_2.10" %  "2.2.0"
    )
  }
}

lazy val maker_ = new Module(
  root = file("maker").asAbsoluteFile,
  name = "maker"
) with ClassicLayout with Bootstrapper {
  override def extraProjectPomInfo = extraPomInfo
  override def organization = Some("com.github.cage433")
  override def resources() = {
    Vector(
      "org.scalatest" % "scalatest_2.10" % "2.2.0",
      "ch.qos.logback" % "logback-classic" % "1.0.6",
      "org.slf4j" % "jcl-over-slf4j" % "1.6.1",
      "commons-io" % "commons-io" % "2.1",
      "com.typesafe.zinc" % "zinc" % "0.3.7",
      "org.apache.httpcomponents" % "httpclient" % "4.3",
      "org.apache.ivy" % "ivy" % "2.3.0-rc2",
      "org.scalaz" % "scalaz-core_2.10" % "7.0.1",
      "com.google.guava" % "guava" %  "11.0.2", 
      "com.typesafe" % "config" % "1.2.1",
      "io.spray" % "spray-json_2.10" % "1.3.1",
      "javax.inject" % "javax.inject" %  "1", 
      "org.apache.commons" % "commons-exec" % "1.3",
      "org.apache.maven" % "maven-aether-provider" % "3.2.5",
      "org.eclipse.aether" % "aether-connector-basic" % "1.0.0.v20140518",
      "org.eclipse.aether" % "aether-impl" % "1.0.0.v20140518",
      "org.eclipse.aether" % "aether-transport-file" % "1.0.0.v20140518",
      "org.eclipse.aether" % "aether-transport-http" % "1.0.0.v20140518",
      "org.mortbay.jetty" % "jetty" % "6.1.26"
    )
  }
}


// Used to disambiguate which maker is running in the repl.
def pwd = println(Properties.userDir)

def publishSnapshots(version : String) = {
  testReporter.publishSonatypeSnapshot(version) andThen maker_.publishSonatypeSnapshot(version)
}

import maker_._
