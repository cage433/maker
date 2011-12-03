import maker.project.Project
import java.io.File

val libRoot = new File("lib_managed")

val managerRoot = new File("manager")
val manager = new Project( 
  "manager",
  managerRoot,
  List(new File(managerRoot, "src")),
  List(new File(managerRoot, "tests")),
  List(new File("lib"), libRoot)
)

val utilsRoot = new File("utils") 
val utils = new Project(
  "utils",
  utilsRoot,
  List(new File(utilsRoot, "src")),
  List(new File(utilsRoot, "tests")),
  List(new File("lib"), libRoot)
) dependsOn (manager)

