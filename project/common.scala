import java.util.Properties
import java.io.File
import maker.utils.FileUtils._

object Common {

  println("\n ** Loading (compiled) common definitions...\n")


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

  lazy val lightweightLaunchArgs = List(
    "-server",
    "-XX:MaxPermSize=1024m",
    "-Xss128k",
    "-Xms2000m",
    "-Xmx4000m",
    "-Dsun.awt.disablegrab=true",
    "-XX:+UseConcMarkSweepGC") ::: {
      if (verboseGC) List(
        "-verbose:gc",
        "-XX:+PrintGCTimeStamps",
        "-XX:+PrintGCDetails")
      else Nil
    }
}
