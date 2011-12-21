import maker.project.Project
import java.io.File

def project(name : String) = Project(
  name, 
  new File(name),
  List(new File(name, "src")),
  List(new File(name, "tests")),
  List(new File(name, "lib_managed"), new File(name, "lib"), new File(name, "maker-lib"))
)
val manager = project("manager")
val utils = project("utils") dependsOn manager
val osgirun = Project(
  "osgirun", 
  new File("osgirun"),
  List(new File("osgirun/src")),
  List(new File("osgirun/tests")),
  List(new File("osgirun/lib_managed"), new File("osgirun/lib"), new File("osgirun/osgi_jars"))
)
val booter = project("booter")
val concurrent = project("concurrent") dependsOn utils
val quantity = project("quantity") dependsOn utils
val osgiManager = project("osgimanager") dependsOn (manager, utils)
val singleClasspathManager = project("singleclasspathmanager") dependsOn (manager, utils, osgiManager)
