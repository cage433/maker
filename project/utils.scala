import java.util.Properties
import java.io.File
import org.apache.commons.io.FileUtils._
import maker.project._
import maker.project.TopLevelProject
import maker.utils.FileUtils._
import maker.utils.MakerLog
import maker.utils.os.Command
import maker.utils.os.Command._
import maker.utils.ModuleId._
import maker.utils.GroupAndArtifact
import maker.task.BuildResult

import Common._

object Utils {

  println("\n ** Loading (compiled) build utils...\n")


  def getProperty(name : String) : Option[String] = {
    val v = System.getProperty(name)
    if (v == null || v.trim.size == 0) None else Some(v.trim)
  }

  def getPropertyOrDefault(name : String, default : String) = {
    val p = getProperty(name)
    val v = p.getOrElse(default)
    println("Property name: '" + name + "' value was: '" + p + "', selected value: '" + v + "'")
    v
  }

  def runCmd(cmd : Command) : Either[Int, String] =
    cmd.exec() match {
      case 0 => println("Process completed"); Right("OK")
      case errNo => println("Process failed"); Left(errNo)
    }

  def runMavenCmd(cwd : File, args : String*) =
    runCmd(Command(MakerLog(), Some(cwd), ("mvn" :: args.toList) : _*))

  def updateIvyFromProjectPom(project : Project) = {
    val antFileName = "antMakeIvy.xml"
    val antFile = file("maker", antFileName)
    val tmpFile = file(project.rootAbsoluteFile, antFileName)
    copyFile(antFile, tmpFile)
    val args = List("ant", "-f", file(project.rootAbsoluteFile, "antMakeIvy.xml").getAbsolutePath)
    val cmd = Command(MakerLog(), Some(project.rootAbsoluteFile), args : _*)
    val r = runCmd(cmd)
    tmpFile.delete
    r
  }

  /**
   * litle bit of pimping of a standard Maker project,
   *  add in some Titan / maven related utils and integration
   */
  case class RichProject(project : Project) {
    def updateIvyFromPom = updateIvyFromProjectPom(project)
    def mvnCompile = runMavenCmd(project.rootAbsoluteFile, "compile")
    def mvnInstall = runMavenCmd(project.rootAbsoluteFile, "install")
    def mvn(cmd : String, args : String*) = runMavenCmd(project.rootAbsoluteFile, (cmd :: args.toList) : _*)
    def rich = this
  }
  object RichProject {
    implicit def toRichProject(project : Project) : RichProject = new RichProject(project)
  }


  import RichProject._

  def writeClasspath(p : Project) {
    val cp = p.compilationClasspath(SourceCompilePhase)
    writeToFile(file("p.name"), "export CLASSPATH=" + cp)
  }

  // handle the build result to output a litle detail to console and return appropriate error codes for caller (i.e. for teamcity reporting etc)
  def handleExit(br : BuildResult) = {
    if (br.succeeded) {
      println("Build OK:\n" + br.result)
      // following line of timed results doesn't work reliably, some task results don't have an keys snapshot for task completed, bug in maker?
      // println("task times : \n" + buildResults.taskCompletedTimes.map(t => (t._1, "took: " + t._2)).mkString("\n"))
      System.exit(0)
    }
    else {
      println("Build Failed, reason: \n" + br.result)
      println("Exiting with -1")
      System.exit(-1)
    }
  }
}
