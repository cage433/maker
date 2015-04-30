package maker.project

import maker.task.BuildResult
import maker.task.tasks._
import maker.utils.FileUtils._
import java.io.File
import maker.utils.RichString._
import scala.collection.immutable.Nil
import maker.task.compile._
import com.typesafe.config.{ConfigFactory, Config}
import scala.xml.{Elem, NodeSeq}
import maker.ScalaVersion

case class Project(
  name : String,
  root : File,
  immediateUpstreamModules:List[Module],
  config : Config = ConfigFactory.load(),
  topLevelExcludedFolders:List[String] = Nil,
  isTestProject : Boolean = false
) 
  extends ProjectTrait 
{

  def projectRoot = root.asAbsoluteFile

  def organization : Option[String] = None
  def artifactId(scalaVersion : ScalaVersion) = s"${name}_${scalaVersion.versionBase}"
  def modules = immediateUpstreamModules
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
  def docOutputDir(scalaVersion : ScalaVersion) = file(rootAbsoluteFile, "docs", scalaVersion.versionNo)
  def packageDir(scalaVersion : ScalaVersion) = file(rootAbsoluteFile, "package", scalaVersion.versionNo)

  def testClassNames(rootProject : ProjectTrait, scalaVersion : ScalaVersion, lastCompilationTime : Option[Long]) = {
    upstreamModules.flatMap(_.testClassNames(rootProject, scalaVersion, lastCompilationTime))
  }

  def publishLocalRootDir  = file(System.getenv("HOME"), ".maker", "publish-local")
  def publishLocalDir(version : String, scalaVersion : ScalaVersion) = file(publishLocalRootDir, organization.getOrElse(???), artifactId(scalaVersion), version).makeDirs
  def publishLocalJarDir(version : String, scalaVersion : ScalaVersion) = file(publishLocalDir(version, scalaVersion), "jars").makeDir
  def publishLocalPomDir(version : String, scalaVersion : ScalaVersion) = file(publishLocalDir(version, scalaVersion), "poms").makeDir
  def publishLocalPomFile(version : String, scalaVersion : ScalaVersion) = file(publishLocalPomDir(version, scalaVersion), s"pom.xml")

  def packageJar(version : Option[String], scalaVersion : ScalaVersion) = {
    val versionAsString = version.map("-" + _).getOrElse("")
    val jarBasename = artifactId(scalaVersion) + versionAsString + ".jar"
    file(packageDir(scalaVersion).getAbsolutePath, jarBasename)
  }

  def sourcePackageJar(version : Option[String], scalaVersion : ScalaVersion) = {
    val versionAsString = version.map("-" + _).getOrElse("")
    val jarBasename = artifactId(scalaVersion) + versionAsString + "-sources.jar"
    file(packageDir(scalaVersion).getAbsolutePath, jarBasename)
  }

  def publishLocalJar(version : String, scalaVersion : ScalaVersion) = 
    file(
      publishLocalJarDir(version, scalaVersion), 
      packageJar(Some(version), scalaVersion).getName)

  def publishLocalSourceJar(version : String, scalaVersion : ScalaVersion) = 
    file(
      publishLocalJarDir(version, scalaVersion), 
      sourcePackageJar(Some(version), scalaVersion).getName)

  def pack(scalaVersion : ScalaVersion) : BuildResult = {
    val tasks = PackageJarTask(this, version = None, scalaVersion = scalaVersion) :: Nil

    execute(transitiveBuild(tasks))
  }
  def pack : BuildResult = pack(defaultScalaVersion)

  def docPackageJar(scalaVersion : ScalaVersion) = file(packageDir(scalaVersion).getAbsolutePath, name + "-javadoc.jar")
  def doc(scalaVersion : ScalaVersion) = execute(transitiveBuild(DocTask(this, scalaVersion) :: Nil))
  def doc : BuildResult = doc(defaultScalaVersion)

  def publishLocalTaskBuild(version : String, signArtifacts : Boolean, scalaVersion : ScalaVersion) = {
    transitiveBuild(PublishLocalTask(this, version, signArtifacts, scalaVersion) :: Nil)
  }

  def publishLocal(version : String, signArtifacts : Boolean, scalaVersion : ScalaVersion) = {
    execute(publishLocalTaskBuild(version, signArtifacts, scalaVersion))
  }

  def extraProjectPomInfo : List[NodeSeq] = Nil

  def bundleJar = file(rootAbsoluteFile, "bundle.jar")

  def publishToSonatypeBuild(version : String, scalaVersion : ScalaVersion) = transitiveBuild(PublishToSonatype(this, version, scalaVersion) :: Nil)
  def publishToSonatype(version : String, scalaVersion : ScalaVersion) = execute(publishToSonatypeBuild(version, scalaVersion))

  def publishSonatypeSnapshotBuild(version : String, scalaVersion : ScalaVersion) = transitiveBuild(PublishSnapshotToSonatype(this, version, scalaVersion) :: Nil)
  def publishSonatypeSnapshot(version : String, scalaVersion : ScalaVersion) = execute(publishSonatypeSnapshotBuild(version, scalaVersion))

  def dependencies = upstreamModules.flatMap(_.dependencies).distinct

  def testTaskBuild(scalaVersion : ScalaVersion, lastCompilationTimeFilter : Option[Long]) = {
    // For a project, `test` runs tests of all modules
    transitiveBuild(
      RunUnitTestsTask(
        s"Unit tests for $this", 
        upstreamModules,
        rootProject = this, 
        classOrSuiteNames_ = None,
        scalaVersion = scalaVersion,
        lastCompilationTimeFilter = lastCompilationTimeFilter
      ) :: Nil
    )
  }


  def testCompileTaskBuild(scalaVersion : ScalaVersion) = transitiveBuild(
    upstreamModules.map(TestCompileTask(this, _, scalaVersion))
  )
}
