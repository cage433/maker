import maker.project.Project
import java.io.File

val managerRoot = new File("manager")
val manager = new Project( 
  "manager",
  managerRoot,
  List(new File(managerRoot, "src")),
  List(new File(managerRoot, "tests")),
  List(new File("lib"), new File("lib_managed"))
)
