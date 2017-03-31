import scala.util.Properties

Properties.setProp("scala.usejavacp", "false")
Properties.setProp("log4j.ignoreTCL", "true")



import scala.xml.NodeSeq
import maker.task.FailingTests._
import maker.task.compile._
import maker.project._
import maker.ScalaVersion
import maker.utils.FileUtils._
import org.eclipse.aether.util.artifact.JavaScopes

import DependencyPimps._

def extraPomInfo : List[NodeSeq] = {
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

lazy val testReporterModule = new Module(
  root = file("test-reporter").asAbsoluteFile, 
  name = "maker-test-reporter",
  scalaVersion = ScalaVersion.TWO_TEN_DEFAULT
) with ClassicLayout with TmuxIntegration {
  override def dependencies() = {
    Vector(
      "org.scalatest" % "scalatest" %%  "3.0.1"
    )
  }
  override def scalacOptions = Seq("-feature", "-deprecation", "-Xfatal-warnings")
}

lazy val testReporterProject = new Project(
  organization = "com.github.cage433",
  name = "maker-test-reporter",
  root = file("test-reporter").asAbsoluteFile, 
  modules = List(testReporterModule),
  scalaVersion = ScalaVersion.TWO_TEN_DEFAULT,
  extraProjectPomInfo = extraPomInfo
)

lazy val makerModule = new Module(
  root = file("maker").asAbsoluteFile,
  name = "maker",
  scalaVersion = ScalaVersion.TWO_TEN_DEFAULT
) with ClassicLayout with Bootstrapper {
  override def dependencies() = {
    Vector(
      "org.scalatest" % "scalatest" %% "3.0.1",
      "ch.qos.logback" % "logback-classic" % "1.2.1",
      "org.slf4j" % "jcl-over-slf4j" % "1.7.24",
      "commons-io" % "commons-io" % "2.5",
      "com.typesafe.zinc" % "zinc" % "0.3.13",
      "org.apache.httpcomponents" % "httpclient" % "4.5.3",
      "org.scalaz" % "scalaz-core" %% "7.2.9",
      "io.spray" % "spray-json_2.10" % "1.3.3",
      "javax.inject" % "javax.inject" %  "1", 
      "com.github.cage433" % "maker-test-reporter" %% "0.44",
      "org.apache.commons" % "commons-exec" % "1.3",
      "org.apache.maven" % "maven-aether-provider" % "3.3.9",
      "org.eclipse.aether" % "aether-connector-basic" % "1.1.0",
      "org.eclipse.aether" % "aether-impl" % "1.1.0",
      "org.eclipse.aether" % "aether-transport-file" % "1.1.0",
      "org.eclipse.aether" % "aether-transport-http" % "1.1.0",
      "org.eclipse.aether" % "aether-test-util" % "1.1.0",
      "com.google.guava" % "guava" % "21.0"
    )
  }
  override def scalacOptions = Seq("-feature", "-deprecation", "-Xfatal-warnings")
}

lazy val makerProject = new Project(
  organization = "com.github.cage433",
  name = "maker", 
  root = file("."), 
  modules = List(makerModule),
  scalaVersion = ScalaVersion.TWO_TEN_DEFAULT,
  extraProjectPomInfo = extraPomInfo
) with TmuxIntegration  
// Used to disambiguate which maker is running in the repl.
def pwd = println(Properties.userDir)

def snapshot(version_ : String) = {
  val version = if (version_.contains("SNAPSHOT"))
    version_
  else 
    s"${version_}-SNAPSHOT"

  testReporterProject.publishSonatypeSnapshot(version) andThen 
  testReporterProject.copy(scalaVersion = ScalaVersion.TWO_ELEVEN_DEFAULT).publishSonatypeSnapshot(version) andThen 
  testReporterProject.copy(scalaVersion = ScalaVersion.TWO_TWELVE_DEFAULT).publishSonatypeSnapshot(version) andThen 
  makerProject.publishSonatypeSnapshot(version)
}

def snapshotTestReporter() = {
  testReporterProject.publishLocal("1.0-SNAPSHOT", signArtifacts = false) 
}

def release(version : String) = {
  testReporterProject.publishToSonatype(version) andThen 
  testReporterProject.copy(scalaVersion = ScalaVersion.TWO_ELEVEN_DEFAULT).publishToSonatype(version) andThen 
  testReporterProject.copy(scalaVersion = ScalaVersion.TWO_TWELVE_DEFAULT).publishToSonatype(version) andThen 
  makerProject.publishToSonatype(version)
}

import makerProject._

def writeVimClasspath = maker.project.VimClasspath.writeVimClasspath(makerProject)
