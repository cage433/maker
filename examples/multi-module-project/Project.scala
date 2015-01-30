import maker.project.{Module, Project}
import java.io.File

val a = new Module(
  root = new File("a"), 
  projectRoot_ = new File("."),
  name = "a") 

val b = new Module(
  root = new File("b"), 
  projectRoot_ = new File("."),
  name = "b",
  immediateUpstreamModules = List(a)) 

val c = new Module(new File("c"), new File("."), "c", List(a)) 
val d = new Module(new File("d"), new File("."), "d", List(c)) 

val project = Project(
  name = "top-level-project",
  root = new File("."),
  immediateUpstreamModules = List(d)
)
