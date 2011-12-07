import maker._
import project.Project

val libRoot = List(file("lib_managed"))

lazy val manager = Project( 
  "manager",
  file("manager"),
  libDirs = libRoot
)

lazy val osgiRun = Project(
  "osgirun",
  file("osgirun"),
  libDirs = libRoot
)

lazy val booter = Project(
  "booter",
  file("booter"),
  libDirs = libRoot
)

lazy val utils = Project(
  "utils",
  file("utils"),
  libDirs = libRoot
) dependsOn (manager)

lazy val concurrent = Project(
  "concurrent",
  file("concurrent"),
  libDirs = libRoot
) dependsOn (utils)

lazy val quantity = Project(
  "quantity",
  file("quantity"),
  libDirs = libRoot
) dependsOn (utils)

lazy val osgiManager = Project(
  "osgimanager",
  file("osgimanager"),
  libDirs = libRoot
) dependsOn(manager, utils)

