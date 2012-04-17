println("\n ** Loading common build definitions...\n")


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

case class RichProject(project : Project) {
  def updateIvyFromPom() = updateIvyFromProjectPom(project)
  def updateSchema() = updateTitanSchema(Some(project))
}
object RichProject {
  implicit def toRichProject(project : Project) : RichProject = new RichProject(project)
}
import RichProject._

def writeClasspath(p : Project) {
  val cp = p.compilationClasspath
  writeToFile(file("p.name"), "export CLASSPATH=" + cp)
}

