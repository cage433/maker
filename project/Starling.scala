import sbt._
import Keys._
import java.io.File

object Dependencies{
  val utilsDependencies = Seq(
    "cglib" % "cglib-nodep" % "2.2" withSources(),
    "joda-time" % "joda-time" % "1.6" withSources(),
    "com.rabbitmq" % "amqp-client" % "1.7.2" withSources(),
    "log4j" % "log4j" % "1.2.16" withSources(),
    "org.slf4j" % "slf4j-log4j12" % "1.6.1" withSources(),
    "com.google.collections" % "google-collections" % "1.0" withSources(),
    "commons-codec" % "commons-codec" % "1.4" withSources(),
    "colt" % "colt" % "1.0.3",
    "com.thoughtworks.xstream" % "xstream" % "1.3.1" withSources(),
    "org.testng" % "testng" % "5.8" classifier "jdk15" withSources(),
    // Test dependencies
    "org.mockito" % "mockito-all" % "1.8.2" % "test" withSources(),
    "org.testng" % "testng" % "5.8" classifier "jdk15" withSources()
  ) 

  val bouncyRmiDependencies = Seq(
    "cglib" % "cglib-nodep" % "2.2" withSources(),
    "org.jboss.netty" % "netty" % "3.2.3.Final" withSources(),
    "commons-io" % "commons-io" % "1.3.2" withSources(),
    "org.scala-lang" % "scala-swing" % "2.9.0-1" withSources()
  ) 

  val authDependencies = Seq("com.sun.jna" % "jna" % "3.0.9" withSources())

  val loopyxlDependencies = Seq(
    "com.google.protobuf" % "protobuf-java" % "2.3.0" withSources()
  ) 

  val guiApiDependencies = Seq( 
    "net.debasishg" % "sjson_2.8.0" % "0.8" intransitive() withSources()
  ) 
}
object StarlingBuild extends Build{

  import Dependencies._

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"),
    unmanagedBase <<= baseDirectory( (base: File) => base /"lib"),
    scalaVersion := "2.9.0-1"
  )

  lazy val root = Project("starling", file("."), settings = standardSettings) aggregate (utils, bouncyrmi, auth, concurrent, quantity, daterange, loopyxl, maths, pivot, pivotUtils)


  lazy val utils = Project(
    "utils", 
    file("./utils"), 
    settings = standardSettings ++ Seq(libraryDependencies ++= utilsDependencies)
  )

  lazy val bouncyrmi = Project(
    "bouncyrmi", 
    file("./bouncyrmi"),
    settings = standardSettings ++ Seq(libraryDependencies ++= bouncyRmiDependencies)
  )
  lazy val auth = Project(
    "auth", 
    file("./auth"),
    settings = standardSettings ++ Seq(libraryDependencies ++= authDependencies)
  ) dependsOn (utils, bouncyrmi)

  lazy val concurrent = Project(
    "concurrent", 
    file("./concurrent"),
    settings = standardSettings
  ) dependsOn (utils) //;w

  lazy val quantity = Project(
    "quantity", 
    file("./quantity"),
    settings = standardSettings
  ) dependsOn (utils)

  lazy val daterange = Project(
    "daterange", 
    file("./daterange"),
    settings = standardSettings
  ) dependsOn(utils)

  lazy val loopyxl = Project(
    "loopyxl", 
    file("./loopyxl"),
    settings = standardSettings ++ Seq(libraryDependencies ++= loopyxlDependencies)
  ) dependsOn(bouncyrmi, auth)

  lazy val maths = Project(
    "maths", 
    file("./maths"),
    settings = standardSettings
  ) dependsOn(quantity, daterange)

  lazy val pivot = Project(
    "pivot", 
    file("./pivot"),
    settings = standardSettings
  ) dependsOn(quantity)

  lazy val pivotUtils = Project(
    "pivot.utils", 
    file("./pivot.utils"),
    settings = standardSettings
  ) dependsOn(daterange, pivot)
}

