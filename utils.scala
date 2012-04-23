println("\n ** Loading build utils...\n")


def runCmd(cmd : Command) : Either[(Int, String), String] =
  cmd.exec() match {
    case res @ (0, msg) => println("Process completed"); Right(msg)
    case err @ (_, _) => println("Process failed: " + err); Left(err)
    case _ => throw new Exception("Unhandled command result")
  }

def runMavenCmd(cwd : File, args : String*) =
  runCmd(Command(Some(cwd), ("mvn" :: args.toList) : _*))

def updateIvyFromProjectPom(project : Project) = {
  val antFileName = "antMakeIvy.xml"
  val antFile = file("maker", antFileName)
  val tmpFile = file(project.root, antFileName)
  copyFile(antFile, tmpFile)
  val args = List("ant", "-f", file(project.root, "antMakeIvy.xml").getAbsolutePath)
  val cmd = Command(Some(project.root), args : _*)
  val r = cmd.exec() match {
    case res @ (0, _) => println("Process completed"); res
    case err @ (_, _) => println("Process failed: " + err); err
  }
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
  Command(Some(file(".").getAbsoluteFile), args : _*).exec() match {
    case res @ (0, _) => println("Process completed"); res
    case err @ (_, _) => println("Process failed: " + err); err
  }
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
}
object RichProject {
  implicit def toRichProject(project : Project) : RichProject = new RichProject(project)
}
import RichProject._

def writeClasspath(p : Project) {
  val cp = p.compilationClasspath
  writeToFile(file("p.name"), "export CLASSPATH=" + cp)
}

