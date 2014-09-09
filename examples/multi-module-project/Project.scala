import maker.project.{Module, Project}
import java.io.File

val a = new Module(
  root = new File("a"), 
  name = "a") with ClassicLayout

val b = new Module(
  root = new File("b"), 
  name = "b",
  immediateUpstreamModules = List(a)) with ClassicLayout

val c = new Module(new File("c"), "c", List(a)) with ClassicLayout
val d = new Module(new File("d"), "d", List(c)) with ClassicLayout

val project = Project(
  name = "top-level-project",
  root = new File("."),
  immediateUpstreamModules = List(d)
)
