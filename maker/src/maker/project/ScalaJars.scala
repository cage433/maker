package maker.project

import maker.task.compile.SourceCompilePhase
import java.io.File

trait ScalaJars {
  self: ProjectTrait =>
    private def findSingleScalaJar(partName : String) : File = {
      dependencyJars(SourceCompilePhase).filter(_.getName.contains(partName)) match {
        case List(jarFile) => jarFile
        case other => throw new IllegalStateException(s"Expected to find a single scala reflect jar, got $other")
      }
    }
    def scalaReflectJar = findSingleScalaJar("scala-reflect")
    def scalaCompilerJar = findSingleScalaJar("scala-compiler")
    def scalaLibraryJar = findSingleScalaJar("scala-library")
    def scalaXmlJar : Option[File] = if (scalaVersion.major >= 11) Some(findSingleScalaJar("scala-xml")) else None
    def scalaParserCombinatorJar : Option[File] = if (scalaVersion.major >= 11) Some(findSingleScalaJar("scala-parser-combinators")) else None
    def scalaJars() = 
      Vector(scalaReflectJar, scalaCompilerJar, scalaLibraryJar) ++: 
        scalaXmlJar.toList ++:
        scalaParserCombinatorJar.toList 
}
