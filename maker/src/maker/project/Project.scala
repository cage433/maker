package maker.project

import maker.task.BuildResult
import maker.task.tasks._
import maker.utils.FileUtils._
import java.io.File
import maker.utils.RichString._
import scala.collection.immutable.Nil
import maker.task.compile._
import scala.xml.{Elem, NodeSeq}
import maker.ScalaVersion

case class Project(
  name : String,
  root : File,
  immediateUpstreamModules:Seq[Module],
  topLevelExcludedFolders:Seq[String] = Nil,
  isTestProject : Boolean = false,
  scalaVersion: ScalaVersion = ScalaVersion.TWO_ELEVEN_DEFAULT,
  organization: Option[String] = None
) 
  extends ProjectTrait 
{

  def projectRoot = root.asAbsoluteFile

  def artifactId = s"${name}_${scalaVersion.versionBase}"
  def furthestDownstreamModules = immediateUpstreamModules
  def testModuleDependencies = upstreamModules
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
  def docOutputDir = file(rootAbsoluteFile, "docs", scalaVersion.versionNo)
  def packageDir = file(rootAbsoluteFile, "package", scalaVersion.versionNo)

  def publishLocalRootDir  = file(System.getenv("HOME"), ".maker", "publish-local")
  def publishLocalDir(version : String) = file(publishLocalRootDir, organization.getOrElse(???), artifactId, version).makeDirs
  def publishLocalJarDir(version : String) = file(publishLocalDir(version), "jars").makeDir
  def publishLocalPomDir(version : String) = file(publishLocalDir(version), "poms").makeDir
  def publishLocalPomFile(version : String) = file(publishLocalPomDir(version), s"pom.xml")

  def packageJar(version : Option[String]) = {
    val versionAsString = version.map("-" + _).getOrElse("")
    val jarBasename = artifactId + versionAsString + ".jar"
    file(packageDir.getAbsolutePath, jarBasename)
  }

  def sourcePackageJar(version : Option[String]) = {
    val versionAsString = version.map("-" + _).getOrElse("")
    val jarBasename = artifactId + versionAsString + "-sources.jar"
    file(packageDir.getAbsolutePath, jarBasename)
  }

  def publishLocalJar(version : String) = 
    file(
      publishLocalJarDir(version), 
      packageJar(Some(version)).getName)

  def publishLocalSourceJar(version : String) = 
    file(
      publishLocalJarDir(version), 
      sourcePackageJar(Some(version)).getName)

  def pack : BuildResult = {
    val tasks = PackageJarTask(this, version = None) :: Nil

    execute(transitiveBuild(tasks))
  }

  def docPackageJar = file(packageDir.getAbsolutePath, name + "-javadoc.jar")
  def doc = execute(transitiveBuild(DocTask(this) :: Nil))

  def publishLocalTaskBuild(version : String, signArtifacts : Boolean) = {
    transitiveBuild(PublishLocalTask(this, version, signArtifacts) :: Nil)
  }

  def publishLocal(version : String, signArtifacts : Boolean) = {
    execute(publishLocalTaskBuild(version, signArtifacts))
  }

  def extraProjectPomInfo : List[NodeSeq] = Nil

  def bundleJar = file(rootAbsoluteFile, "bundle.jar")

  def publishToSonatypeBuild(version : String) = transitiveBuild(PublishToSonatype(this, version) :: Nil)
  def publishToSonatype(version : String) = execute(publishToSonatypeBuild(version))

  def publishSonatypeSnapshotBuild(version : String) = transitiveBuild(PublishSnapshotToSonatype(this, version) :: Nil)
  def publishSonatypeSnapshot(version : String) = execute(publishSonatypeSnapshotBuild(version))

  def dependencies = upstreamModules.flatMap(_.dependencies).distinct

  def testTaskBuild(lastCompilationTimeFilter : Option[Long]) = {
    // For a project, `test` runs tests of all modules
    transitiveBuild(
      RunUnitTestsTask(
        s"Unit tests for $this", 
        upstreamModules,
        rootProject = this, 
        classOrSuiteNames_ = None,
        lastCompilationTimeFilter = lastCompilationTimeFilter
      ) :: Nil
    )
  }


  def testCompileTaskBuild(testPhases : Seq[CompilePhase]) = transitiveBuild(
    upstreamModules.flatMap{module => 
      testPhases.map(CompileTask(this, module, _))
    }
  )
}
