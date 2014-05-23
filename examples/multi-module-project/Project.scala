import maker.project.{Module, Project}
import java.io.File

val a = new Module(
  root = new File("a"), 
  name = "a")

val b = new Module(
  root = new File("b"), 
  name = "b",
  immediateUpstreamModules = List(a))

val c = new Module(new File("c"), "c", List(a))
val d = new Module(new File("d"), "d", List(c))

val project = Project(
  name = "top-level-project",
  root = new File("."),
  immediateUpstreamModules = List(d)
)
