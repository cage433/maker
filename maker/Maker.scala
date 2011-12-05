import maker.project.Project
import java.io.File

val libRoot = new File("lib_managed")

val managerRoot = new File("manager")
val manager = new Project( 
  "manager",
  managerRoot,
  List(new File(managerRoot, "src")),
  List(new File(managerRoot, "tests")),
  List(libRoot)
)

val utilsRoot = new File("utils") 
val utils = new Project(
  "utils",
  utilsRoot,
  List(new File(utilsRoot, "src")),
  List(new File(utilsRoot, "tests")),
  List(libRoot)
) dependsOn (manager)

val concurrentRoot = new File("concurrent")
val concurrent = new Project(
  "concurrent",
  concurrentRoot,
  List(new File(concurrentRoot, "src")),
  List(new File(concurrentRoot, "tests")),
  List(libRoot)
) dependsOn (utils)

