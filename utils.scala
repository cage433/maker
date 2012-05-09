println("\n ** Loading build utils...\n")


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
  runCmd(Command(Some(cwd), ("mvn" :: args.toList) : _*))

def updateIvyFromProjectPom(project : Project) = {
  val antFileName = "antMakeIvy.xml"
  val antFile = file("maker", antFileName)
  val tmpFile = file(project.root, antFileName)
  copyFile(antFile, tmpFile)
  val args = List("ant", "-f", file(project.root, "antMakeIvy.xml").getAbsolutePath)
  val cmd = Command(Some(project.root), args : _*)
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
  val cmd = Command(Some(file(".").getAbsoluteFile), args : _*)
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
}
object RichProject {
  implicit def toRichProject(project : Project) : RichProject = new RichProject(project)
}
import RichProject._

def writeClasspath(p : Project) {
  val cp = p.compilationClasspath
  writeToFile(file("p.name"), "export CLASSPATH=" + cp)
}

