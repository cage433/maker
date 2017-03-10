package maker

import scalaz.syntax.std.ToBooleanOps
import maker.utils.Int
import maker.utils.FileUtils._
import java.io.File
import org.eclipse.aether.graph.{Exclusion, Dependency => AetherDependency}
import maker.project.{DependencyPimps, RichDependency}

/*
 * Deterimines which scala libraries are project
 * dependencies
 *
 * Version is 2.<major>.<minor>
 */
case class ScalaVersion(
  major : Int, 
  minor : Int
)
  extends DependencyPimps 
  with ToBooleanOps
{
  require( 
    List(10, 11, 12).contains(major),
    "Only supports scala 2.10 to 2.12 inclusive"
  )
  val versionNo = s"2.$major.$minor"
  val versionBase = s"2.$major"
  override def toString = versionNo

  def scalaLibraryRichDependency = "org.scala-lang" % s"scala-library" % versionNo
  def scalaCompilerRichDependency = "org.scala-lang" % s"scala-compiler" % versionNo
  def scalaReflectRichDependency = "org.scala-lang" % s"scala-reflect" % versionNo
  def scalaXmlRichDependency : Option[RichDependency] = (major >= 11).option("org.scala-lang.modules" % s"scala-xml_$versionBase" % "1.0.3")
  def scalaParserCombinatorRichDependency = (major >= 11).option("org.scala-lang.modules" % s"scala-parser-combinators_$versionBase" %"1.0.3")

  private def scalaRichDependencies = List(scalaLibraryRichDependency, scalaCompilerRichDependency, scalaReflectRichDependency) ::: 
    scalaXmlRichDependency.toList ::: scalaParserCombinatorRichDependency.toList
}

object ScalaVersion{
  def apply(version : String) : ScalaVersion = {
    val regex = """2\.(\d+)\.(\d+)""".r
    version match {
      case regex(major, minor) => 
        ScalaVersion(major.toInt, minor.toInt)
      case _ => 
        throw new IllegalStateException(s"Unrecognised scala version'$version'")
    }
  }

  def majorVersion(scalaVersion : String) = scalaVersion.split('.').take(2).mkString(".")

  val TWO_TEN_DEFAULT = ScalaVersion("2.10.4")
  val TWO_ELEVEN_DEFAULT = ScalaVersion("2.11.8")
  val TWO_TWELVE_DEFAULT = ScalaVersion("2.12.1")
}
