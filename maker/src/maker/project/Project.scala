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
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.util.artifact.SubArtifact
import maker.utils.MakerTestResults

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

  def extraJars = upstreamModules.flatMap(_.extraJars)
  def extraTestSystemProperties = testUpstreamModules.flatMap(_.extraTestSystemProperties)

  def artifactId = s"${name}_${scalaVersion.versionBase}"
  def jarArtifact(version: String) = new DefaultArtifact(organization, artifactId, "", "jar", version)
  def srcArtifact(version: String) = new DefaultArtifact(organization, artifactId, "sources", "jar", version)
  def docArtifact(version: String) = new DefaultArtifact(organization, artifactId, "javadoc", "jar", version)
  def pomArtifact(version: String) = new SubArtifact(jarArtifact(version), "", "pom" )

  lazy val aetherSystem = new AetherSystem(resourceCacheDirectory)
  def publishedLocalJar(version: String) = {
    aetherSystem.absolutePathForLocalArtifact(jarArtifact(version))
  }

  def publishedLocalSourcesJar(version: String) = {
    aetherSystem.absolutePathForLocalArtifact(srcArtifact(version))
  }

  def publishedLocalJavadocJar(version: String) = {
    aetherSystem.absolutePathForLocalArtifact(docArtifact(version))
  }

  def publishedLocalPom(version: String) = {
    aetherSystem.absolutePathForLocalArtifact(pomArtifact(version))
  }

  def publishedLocalDirectory(version: String): File = {
    publishedLocalJar(version).dirname
  }

  def upstreamModules : Seq[Module] = transitiveClosure(modules, {m : Module => m.compileDependencies}).distinct
  def testUpstreamModules : Seq[Module] = transitiveClosure(modules, {m : Module => m.compileDependencies ++ m.testDependencies}).distinct
  def testDependencies = upstreamModules
  override def toString = name

  def docOutputDir = file(rootAbsoluteFile, "docs", scalaVersion.versionNo)
  def packageDir = file(rootAbsoluteFile, "package", scalaVersion.versionNo)

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

  def publishLocalJar(version : String) = 
    file(
      publishLocalJarDir(version), 
      packageJar(Some(version)).getName)


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
       upstreamModules.map{
         module => 
          RunUnitTestsTask(
            s"Unit tests for $this", 
            module,
            rootProject = this, 
            classNamesOrPhase = Right(testPhase),
            lastCompilationTimeFilter = lastCompilationTimeFilter
          )
       }
      ) 
  }


  def testResults = {
    // Test results may either be in a top level project's directory, or else in
    // module directoriy(s)
    upstreamModules.distinct.map(MakerTestResults(_)).reduce(_++_)
  }

  def compileTaskBuild(phases: Seq[CompilePhase]): Build = transitiveBuild(upstreamModules.flatMap{m => phases.map{ p => CompileTask(this, m, p)}})

}
