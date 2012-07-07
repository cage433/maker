import java.util.Properties
import java.io.File
import maker.utils.FileUtils._
import maker.Props
import maker.RichProperties._

object Common {

  println("\n ** Loading common definitions...\n")

  //val scalaVersion = ScalaVersion("2.9.1")

  lazy val unmanagedGlobalProperties : Properties = file("developer.conf")

  val verboseGC = false

  def targetDirFile(name : String) : Option[File] = Some(file(name, "target-maker"))

  lazy val commonLaunchArgs = List(
    "-server",
    "-XX:MaxPermSize=1024m",
    "-Xss128k",
    "-Xms6000m",
    "-Xmx12000m",
    "-Dsun.awt.disablegrab=true",
    "-XX:+UseConcMarkSweepGC") ::: {
      if (verboseGC) List(
        "-verbose:gc",
        "-XX:+PrintGCTimeStamps",
        "-XX:+PrintGCDetails")
      else Nil
    }
}

