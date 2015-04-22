package maker.project

import maker.task.BuildResult
import maker.task.tasks._
import maker.utils.FileUtils._
import java.io.File
import maker.utils.RichString._
import scala.collection.immutable.Nil
import maker.task.compile.{SourceCompilePhase, CompilePhase, TestCompilePhase}
import com.typesafe.config.{ConfigFactory, Config}
import scala.xml.{Elem, NodeSeq}

case class Project(
  name : String,
  root : File,
  immediateUpstreamModules:List[Module],
  config : Config = ConfigFactory.load(),
  topLevelExcludedFolders:List[String] = Nil
) extends TmuxIntegration{

  def projectRoot = root.asAbsoluteFile

  def organization : Option[String] = None
  def artifactId = name
  def modules = immediateUpstreamModules
  //val upstreamModulesForBuild = allUpstreamModules
  override def toString = name

  override def constructorCodeAsString : String = {
    val b = new StringBuffer
    upstreamModules.foreach{
      m => 
        b.addLine(m.constructorCodeAsString)
    }
    b.addLine(s"""val $name = Project($name, file("${root.getAbsolutePath.toString}"), ${upstreamModules.mkString("List(", ", ", ")")})""")
    b.toString
  }
  def docOutputDir = file(rootAbsoluteFile, "docs")
  def packageDir = file(rootAbsoluteFile, "package")

  def testClassNames() = {
    upstreamModules.flatMap(_.testClassNames())
  }

  def publishLocalRootDir  = file(System.getenv("HOME"), ".maker", "publish-local")
  def publishLocalDir(version : String) = file(publishLocalRootDir, organization.getOrElse(???), artifactId, version).makeDirs
  def publishLocalJarDir(version : String) = file(publishLocalDir(version), "jars").makeDir
  def publishLocalPomDir(version : String) = file(publishLocalDir(version), "poms").makeDir
  def publishLocalPomFile(version : String) = file(publishLocalPomDir(version), s"pom.xml")

  def packageJar(version : Option[String]) = {
    val versionAsString = version.map("-" + _).getOrElse("")
    val jarBasename = name + versionAsString + ".jar"
    file(packageDir.getAbsolutePath, jarBasename)
  }

  def sourcePackageJar(version : Option[String]) = {
    val versionAsString = version.map("-" + _).getOrElse("")
    val jarBasename = name + versionAsString + "-sources.jar"
    file(packageDir.getAbsolutePath, jarBasename)
  }

  def publishLocalJar(version : String) = file(publishLocalJarDir(version), packageJar(Some(version)).getName)
  def publishLocalSourceJar(version : String) = file(publishLocalJarDir(version), sourcePackageJar(Some(version)).getName)

  def pack : BuildResult = {
    val tasks = PackageJarTask(this, version = None) :: Nil

    execute(transitiveBuild(tasks))
  }
  def docPackageJar = file(packageDir.getAbsolutePath, name + "-javadoc.jar")
  def doc = execute(transitiveBuild(DocTask(this) :: Nil))

  def publishLocalTaskBuild(version : String, signArtifacts : Boolean) = {
    transitiveBuild(PublishLocalTask(this, version, signArtifacts) :: Nil)
  }

  def publishLocal(version : String, signArtifacts : Boolean = false) = {
    execute(publishLocalTaskBuild(version, signArtifacts))
  }

  def extraProjectPomInfo : List[NodeSeq] = Nil

  def bundleJar = file(rootAbsoluteFile, "bundle.jar")

  def publishToSonatypeBuild(version : String) = transitiveBuild(PublishToSonatype(this, version) :: Nil)
  def publishToSonatype(version : String) = execute(publishToSonatypeBuild(version))

  def publishSonatypeSnapshotBuild(version : String) = transitiveBuild(PublishSnapshotToSonatype(this, version) :: Nil)
  def publishSonatypeSnapshot(version : String) = execute(publishSonatypeSnapshotBuild(version))

  def classpathSansScalaLibs(phase : CompilePhase) : String = {
    var dirs : Seq[File] = upstreamModules.flatMap{
      module => 
        Vector(module.resourceDir(SourceCompilePhase), module.outputDir(SourceCompilePhase), module.managedResourceDir)
    }
    if (phase == TestCompilePhase)
      dirs ++= upstreamTestModules.flatMap{
        module => 
          Vector(module.resourceDir(TestCompilePhase), module.outputDir(TestCompilePhase))
      }
    Module.asClasspathStr(dirs)
  }

  def resources = upstreamModules.flatMap(_.resources).distinct
}
