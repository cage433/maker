package maker

import scalaz.syntax.std.ToBooleanOps
import com.typesafe.config.{ConfigFactory, Config}
import maker.utils.Int
import maker.utils.FileUtils._
import java.io.File
import org.eclipse.aether.graph.{Exclusion, Dependency => AetherDependency}
import maker.project.DependencyPimps

/*
 * Deterimines which scala libraries are project
 * dependencies
 *
 * Version is 2.<major>.<minor>
 */
case class ScalaVersion(
  major : Int, 
  minor : Int,
  config : Config = ConfigFactory.load()) 
  extends ConfigPimps 
  with DependencyPimps 
  with ToBooleanOps
{
  require( 
    List(9, 10, 11).contains(major),
    "Only supports scala 2.9 to 2.11 inclusive"
  )
  private val versionNo = s"2.$major.$minor"
  private val versionBase = s"2.$major"
  override def toString = versionNo

  private def resource(org : String, artifact : String, version : String = versionNo) : AetherDependency = {
    org % artifact % version 
  }
  def scalaLibraryResource = resource("org.scala-lang", "scala-library")
  def compilerResource = resource("org.scala-lang", "scala-compiler")
  def reflectResource = (major >= 10).option(resource("org.scala-lang", "scala-reflect"))
  def xmlResource = (major >= 11).option(resource("org.scala-lang.modules", s"scala-xml_$versionBase", "1.0.3"))
  def parserCombinatorResource = (major >= 11).option(resource("org.scala-lang.modules", s"scala-parser-combinators_$versionBase", "1.0.3"))

  private def resources = List(scalaLibraryResource, compilerResource) ::: reflectResource.toList ::: xmlResource.toList ::: parserCombinatorResource.toList
  private def sourceResources = resources.map(_.sourceDependency)

  def scalaLibraryJar = file(
    config.projectScalaLibDirectory,
    scalaLibraryResource.basename
  )
  def scalaCompilerJar = file(
    config.projectScalaLibDirectory,
    compilerResource.basename
  )

  def scalaReflectJar = reflectResource.map{
    resource => 
      file(
        config.projectScalaLibDirectory,
        resource.basename
      )
  }

  def scalaJars : List[File] = resources.map{
      res => 
        file(
          config.projectScalaLibDirectory,
          res.basename
        )
  }
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
}
