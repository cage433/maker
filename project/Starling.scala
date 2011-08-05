import sbt._
import Keys._
import java.io.File

object StarlingBuild extends Build{
  lazy val root = Project("starling", file(".")) aggregate (utils, bouncyrmi, auth, concurrent, quantity, daterange)


  lazy val utils = Project("utils", file("./utils")) settings (unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"), unmanagedBase <<= baseDirectory( (base: File) => base /"jars"))
  lazy val bouncyrmi = Project("bouncyrmi", file("./bouncyrmi")) settings (
    unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"), 
    unmanagedBase <<= baseDirectory( (base: File) => base /"jars"),
    unmanagedClasspath in Compile <+= (baseDirectory) map { bd => Attributed.blank(bd / "../lib/scala/scala-2.8.1.final/lib/scala-swing.jar")}
  )
  lazy val auth = Project("auth", file("./auth")) settings (unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"), unmanagedBase <<= baseDirectory( (base: File) => base /"jars")) dependsOn (utils, bouncyrmi)
  lazy val concurrent = Project("concurrent", file("./concurrent")) settings (unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"), unmanagedBase <<= baseDirectory( (base: File) => base /"jars")) dependsOn (utils)
  lazy val quantity = Project("quantity", file("./quantity")) settings (unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"), unmanagedBase <<= baseDirectory( (base: File) => base /"jars")) dependsOn (utils)
  lazy val daterange = Project("daterange", file("./daterange")) settings (unmanagedSourceDirectories in Compile <+= baseDirectory(_/"src"), unmanagedBase <<= baseDirectory( (base: File) => base /"jars")) dependsOn (utils)

  //lazy val starling = Project("starling", file(".")) dependsOn (utils)
}

