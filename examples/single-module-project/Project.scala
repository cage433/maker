import maker.project.Module
import maker.ScalaVersion

val singleModule = new Module(
  root = new java.io.File("."), 
  name = "single-module"
) {
  override def dependencies = Vector(
    "org.scalatest" % "scalatest_2.10" %  "2.2.0"
  )
  override def defaultScalaVersion = ScalaVersion.TWO_TEN_DEFAULT
}

