import sbt._
import Keys._
import java.io.File

object StarlingBuild extends Build{

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"),
    unmanagedBase <<= baseDirectory( (base: File) => base /"lib"),
    scalaVersion := "2.9.0-1"
  )

  lazy val root = Project("starling", file(".")) aggregate (utils, bouncyrmi, auth, concurrent, quantity, daterange)

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

  lazy val utils = Project(
    "utils", 
    file("./utils"), 
    settings = standardSettings ++ Seq(libraryDependencies ++= utilsDependencies)
  )
    
  val bouncyRmiDependencies = Seq(
      "cglib" % "cglib-nodep" % "2.2" withSources(),
      "org.jboss.netty" % "netty" % "3.2.3.Final" withSources(),
      "commons-io" % "commons-io" % "1.3.2" withSources(),
      "org.scala-lang" % "scala-swing" % "2.9.0-1" withSources()
    ) 
  lazy val bouncyrmi = Project(
    "bouncyrmi", 
    file("./bouncyrmi"),
    settings = standardSettings ++ Seq(libraryDependencies ++= bouncyRmiDependencies)
  )
  lazy val auth = Project("auth", file("./auth")) settings (unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"), unmanagedBase <<= baseDirectory( (base: File) => base /"jars")) dependsOn (utils, bouncyrmi)
  lazy val concurrent = Project("concurrent", file("./concurrent")) settings (unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"), unmanagedBase <<= baseDirectory( (base: File) => base /"jars")) dependsOn (utils)
  lazy val quantity = Project("quantity", file("./quantity")) settings (unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"), unmanagedBase <<= baseDirectory( (base: File) => base /"jars")) dependsOn (utils)
  lazy val daterange = Project("daterange", file("./daterange")) settings (unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"), unmanagedBase <<= baseDirectory( (base: File) => base /"jars")) dependsOn (utils)

  //lazy val starling = Project("starling", file(".")) dependsOn (utils)
}

