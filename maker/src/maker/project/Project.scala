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

case class Project(
  name : String,
  root : File,
  immediateUpstreamModules:List[Module],
  config : Config = ConfigFactory.load(),
  topLevelExcludedFolders:List[String] = Nil,
  isTestProject : Boolean = false
) extends TmuxIntegration {

  def projectRoot = root.asAbsoluteFile

  def organization : Option[String] = None
  def artifactId = name
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
  def docOutputDir = file(rootAbsoluteFile, "docs")
  def packageDir = file(rootAbsoluteFile, "package")

  def testClassNames(rootProject : ProjectTrait, majorScalaVersion : String) = {
    upstreamModules.flatMap(_.testClassNames(rootProject, majorScalaVersion))
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

  def pack(majorScalaVersion : String) : BuildResult = {
    val tasks = PackageJarTask(this, version = None, majorScalaVersion = majorScalaVersion) :: Nil

    execute(transitiveBuild(tasks))
  }
  def pack : BuildResult = pack(defaultMajorScalaVersion)

  def docPackageJar = file(packageDir.getAbsolutePath, name + "-javadoc.jar")
  def doc(majorScalaVersion : String) = execute(transitiveBuild(DocTask(this, majorScalaVersion) :: Nil))
  def doc : BuildResult = doc(defaultMajorScalaVersion)

  def publishLocalTaskBuild(version : String, signArtifacts : Boolean, majorScalaVersion : String) = {
    transitiveBuild(PublishLocalTask(this, version, signArtifacts, majorScalaVersion) :: Nil)
  }

  def publishLocal(version : String, signArtifacts : Boolean, majorScalaVersion : String) = {
    execute(publishLocalTaskBuild(version, signArtifacts, majorScalaVersion))
  }

  def extraProjectPomInfo : List[NodeSeq] = Nil

  def bundleJar = file(rootAbsoluteFile, "bundle.jar")

  def publishToSonatypeBuild(version : String, majorScalaVersion : String) = transitiveBuild(PublishToSonatype(this, version, majorScalaVersion) :: Nil)
  def publishToSonatype(version : String, majorScalaVersion : String) = execute(publishToSonatypeBuild(version, majorScalaVersion))

  def publishSonatypeSnapshotBuild(version : String, majorScalaVersion : String) = transitiveBuild(PublishSnapshotToSonatype(this, version, majorScalaVersion) :: Nil)
  def publishSonatypeSnapshot(version : String, majorScalaVersion : String) = execute(publishSonatypeSnapshotBuild(version, majorScalaVersion))

  def dependencies = upstreamModules.flatMap(_.dependencies).distinct

  def testTaskBuild(majorScalaVersion : String) = {
    // For a project, `test` runs tests of all modules
    transitiveBuild(
      RunUnitTestsTask(
        s"Unit tests for $this", 
        upstreamModules,
        rootProject = this, 
        classOrSuiteNames_ = None,
        majorScalaVersion = majorScalaVersion
      ) :: Nil
    )
  }

  def test(majorScalaVersion : String) : BuildResult = {
    execute(testTaskBuild(majorScalaVersion))
  }

  def test : BuildResult = test(defaultMajorScalaVersion)


  def testCompileTaskBuild(majorScalaVersion : String) = transitiveBuild(
    upstreamModules.map(TestCompileTask(this, _, majorScalaVersion))
  )
}
