import maker.project.{Module, Project}
import java.io.File
import maker.ScalaVersion

val a = new Module(
  root = new File("a"), 
  name = "a") 
{
  override def dependencies = Vector(
    "org.scalatest" % "scalatest_2.10" %  "2.2.0"
  )
}

val b = new Module(
  root = new File("b"), 
  name = "b",
  compileDependencies = List(a)) 

val c = new Module(new File("c"), "c", compileDependencies = List(a)) 
val d = new Module(new File("d"), "d", compileDependencies = List(c)) 

val project = new Project(
  name = "top-level-project",
  root = new File("."), 
  modules = List(d),
  scalaVersion = ScalaVersion.TWO_TEN_DEFAULT
)
