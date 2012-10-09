import java.util.Properties
import java.io.File
import org.apache.log4j.Level._
import org.apache.commons.io.FileUtils._
import maker.project._
import maker.project.TopLevelProject
import maker.Props
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
    val tmpFile = file(project.root, antFileName)
    copyFile(antFile, tmpFile)
    val args = List("ant", "-f", file(project.root, "antMakeIvy.xml").getAbsolutePath)
    val cmd = Command(MakerLog(), Some(project.root), args : _*)
    val r = runCmd(cmd)
    tmpFile.delete
    r
  }

  def updateTitanSchema(project : Option[Project] = None) {
    val envName = Option(unmanagedGlobalProperties.getProperty("titan.env.name")).getOrElse{
      throw new Exception("Missing env name, property = titan.env.name")
    }
    val scriptDir = file("../../bin/")
    val script = file(scriptDir, "deploy2db")
    println("script = " + script.getAbsolutePath + ", exists = " + script.exists)
    println("updating %s for env %s".format(project.map(_.name).getOrElse("None"), envName))
    val args = "../../bin/" + script.getName :: ("-e" + envName) :: project.toList.map(p => ("-c" + p.name))
    val cmd = Command(MakerLog(), Some(file(".").getAbsoluteFile), args : _*)
    runCmd(cmd)
  }
  def updateAllTitanSchemas() = updateTitanSchema()

  /**
   * litle bit of pimping of a standard Maker project,
   *  add in some Titan / maven related utils and integration
   */
  case class RichProject(project : Project) {
    def updateIvyFromPom = updateIvyFromProjectPom(project)
    def updateSchema = updateTitanSchema(Some(project))
    def mvnCompile = runMavenCmd(project.root, "compile")
    def mvnInstall = runMavenCmd(project.root, "install")
    def mvn(cmd : String, args : String*) = runMavenCmd(project.root, (cmd :: args.toList) : _*)
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
