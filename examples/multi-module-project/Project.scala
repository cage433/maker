import maker.project.{Module, Project}
import java.io.File

val a = new Module(
  root = new File("a"), 
  name = "a") 
{
  override def resources = Vector(
    "org.scalatest" % "scalatest_2.10" %  "2.2.0"
  )
}

val b = new Module(
  root = new File("b"), 
  name = "b",
  immediateUpstreamModules = List(a)) 

val c = new Module(new File("c"), "c", immediateUpstreamModules = List(a)) 
val d = new Module(new File("d"), "d", immediateUpstreamModules = List(c)) 

val project = Project(
  name = "top-level-project",
  root = new File("."), 
  immediateUpstreamModules = List(d)
)
