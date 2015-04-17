import maker.project.Module

val singleModule = new Module(
  root = new java.io.File("."), 
  name = "single-module"
) {
  override def resources = Vector(
    "org.scalatest" % "scalatest_2.10" %  "2.2.0"
  )
}

