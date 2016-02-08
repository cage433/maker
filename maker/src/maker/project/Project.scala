package maker.project

import maker.task.{BuildResult, Build}
import maker.task.tasks._
import maker.utils.FileUtils._
import java.io.File
import maker.utils.RichString._
import scala.collection.immutable.Nil
import maker.task.compile._
import scala.xml.{Elem, NodeSeq}
import maker.ScalaVersion
import org.eclipse.aether.artifact.{Artifact, DefaultArtifact}
import org.eclipse.aether.installation.InstallRequest
import org.eclipse.aether.util.artifact.SubArtifact
import org.eclipse.aether.internal.impl.SimpleLocalRepositoryManager

case class Project(
  organization: String,
  name: String,
  root: File,
  modules:Seq[Module],
  scalaVersion: ScalaVersion = ScalaVersion.TWO_ELEVEN_DEFAULT,
  extraProjectPomInfo : List[NodeSeq] = Nil
) 
  extends ProjectTrait 
{

  def projectRoot = root.asAbsoluteFile

  def artifactId = s"${name}_${scalaVersion.versionBase}"
  def upstreamModules : Seq[Module] = transitiveClosure(modules, {m : Module => m.compileDependencies})
  def testDependencies = upstreamModules
  override def toString = name

  def docOutputDir = file(rootAbsoluteFile, "docs", scalaVersion.versionNo)
  def packageDir = file(rootAbsoluteFile, "package", scalaVersion.versionNo)

  def publishLocalRootDir  = file(System.getenv("HOME"), ".maker", "publish-local")
  def publishLocalDir(version : String) = {
    val rootDir = file(System.getenv("HOME"), ".maker", "resource-cache")
    val relativePath = organization.split('.') :+ artifactId :+ version
    file(rootDir, relativePath: _*)
  }

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

  def bundleJar = file(rootAbsoluteFile, "bundle.jar")

  def publishToSonatypeBuild(version : String) = transitiveBuild(PublishToSonatype(this, version) :: Nil)
  def publishToSonatype(version : String) = execute(publishToSonatypeBuild(version))

  def publishSonatypeSnapshotBuild(version : String) = transitiveBuild(PublishSnapshotToSonatype(this, version) :: Nil)
  def publishSonatypeSnapshot(version : String) = execute(publishSonatypeSnapshotBuild(version))

  def dependencies = upstreamModules.flatMap(_.dependencies).distinct

  def testTaskBuild(testPhase: TestPhase, lastCompilationTimeFilter : Option[Long]) = {
    // For a project, `test` runs tests of all modules
    transitiveBuild(
      RunUnitTestsTask(
        s"Unit tests for $this", 
        upstreamModules,
        rootProject = this, 
        classNamesOrPhase = Right(testPhase),
        lastCompilationTimeFilter = lastCompilationTimeFilter
      ) :: Nil
    )
  }

  def compileTaskBuild(phases: Seq[CompilePhase]): Build = transitiveBuild(modules.flatMap{m => phases.map{ p => CompileTask(this, m, p)}})

  //def testCompileTaskBuild(testPhases : Seq[CompilePhase]) = transitiveBuild(
    //upstreamModules.flatMap{module => 
      //testPhases.map(CompileTask(this, module, _))
    //}
  //)
}
