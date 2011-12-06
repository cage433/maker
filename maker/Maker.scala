import maker._
import project.Project

val libRoot = List(file("lib_managed"))

val managerRoot = file("manager")
val manager = new Project( 
  "manager",
  managerRoot,
  Nil, Nil,
  libRoot
)

val utilsRoot = file("utils") 
val utils = new Project(
  "utils",
  utilsRoot,
  Nil, Nil,
  libRoot
) dependsOn (manager)

val concurrentRoot = file("concurrent")
val concurrent = new Project(
  "concurrent",
  concurrentRoot,
  Nil, Nil,
  libRoot
) dependsOn (utils)

