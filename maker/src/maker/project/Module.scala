package maker.project

import sbt.inc.Analysis
import maker.task._
import maker.task.tasks._
import java.util.concurrent.ConcurrentHashMap
import java.io.File
import org.slf4j.LoggerFactory
import maker.{MakerProps, PomUtils, Resource}
import maker.task.compile._
import scala.collection.immutable.Nil
import sbt.ConsoleLogger
import maker.utils.FileUtils._

/**
  * Corresponds to a module in IntelliJ
  */

class Module(
    val root : File,
    val name : String,
    val immediateUpstreamModules : List[Module] = Nil,
    val immediateUpstreamTestModules : List[Module] = Nil,
    val props : MakerProps = MakerProps(),
    val analyses : ConcurrentHashMap[File, Analysis] = Module.analyses
)
  extends BaseProject
  with TmuxIntegration
{

  import Module.logger
  protected val upstreamModulesForBuild = List(this)

  val resourcesFile = file(root, "external-resources")

  def resources() : List[Resource]  = {
    
    val resources = resourcesFile.readLines.toList.filterNot{
      line => 
        line.startsWith("#") || line.trim.size == 0
    }.map(Resource.parse(_, resourceVersions = props.resourceVersions(), downloadDirectory = Some(managedLibDir)))
    resources.distinct
  }

  def sourceJarResources() : List[Resource] = resources().collect{
    case r if r.isBinaryJarResource => r.copy(classifier = Some("sources"), downloadDirectory = Some(managedLibSourceDir))
  }



  Module.warnOfUnnecessaryDependencies(this)

  def javacOptions : List[String] = Nil 
  def scalacOptions : List[String] = Nil
  /**
   * The standard equals method was slow, making Dependency operations very expensive.
   */
   override def equals(rhs : Any) = {
     rhs match {
       case p : Module if p.root == root => {
         //I believe this assertion should always hold. It's really here so that
         //this overriden equals method never returns true on differing modules.
         assert(this eq p, "Shouldn't have two modules pointing to the same root")
         true
       }
       case _ => false
     }
   }

  override def hashCode = root.hashCode

  private def warnOfRedundantDependencies() {
    immediateUpstreamModules.foreach{
      module =>
        val otherUpstreamModules = immediateUpstreamModules.filterNot(_ == module)
        otherUpstreamModules.find(_.allUpstreamModules.contains(module)) match {
          case Some(otherUpstreamModule) =>
          logger.warn(name + " shouldn't depend on " + module.name + " as it is inherited via " + otherUpstreamModule.name)
          case None =>
        }
    }
  }

  warnOfRedundantDependencies()

  def pomDependencyXML(version : String) = PomUtils.dependencyXml(groupId, artifactId, version)
  def testCompilePhase = ModuleCompilePhase(this, TestCompilePhase)
  def compilePhase = ModuleCompilePhase(this, SourceCompilePhase)


  lazy val allUpstreamModules         : List[Module] = this :: allStrictlyUpstreamModules
  lazy val allUpstreamTestModules         : List[Module] = this :: allStrictlyUpstreamTestModules
  private lazy val allStrictlyUpstreamTestModules : List[Module] = immediateUpstreamTestModules.flatMap(_.allUpstreamTestModules).distinct.sortWith(_.name < _.name)

  override def toString = name

  /**************************
  *       TASKS
  **************************/

  /**
    * Execute just this single task, none of its upstream 
    * dependencies. 
    * In general should only be used by devs at the REPL - should
    * not be used as the basis for more complex builds
    */
  private def executeSansDependencies(task : Task) : BuildResult = {
    val build = Build(
      task.name + " for " + name + " only",
      Dependency.Graph(task),
      props.NumberOfTaskThreads()
    )
    execute(build)
  }

  def cleanOnly = executeSansDependencies(CleanTask(this))
  def testOnly(verbose : Boolean) : BuildResult = executeWithDependencies(RunUnitTestsTask(this, verbose))
  def testOnly :BuildResult = testOnly(false)
  def testFailuredSuitesOnly(verbose : Boolean) : BuildResult = executeSansDependencies(
    RunUnitTestsTask.failingTests(this, verbose)
  )
  def testFailuredSuitesOnly : BuildResult = testFailuredSuitesOnly(false)
  def updateOnly = executeSansDependencies(UpdateTask(this, forceSourceUpdate = false))


  /********************
  *     Test classses 
  ********************/


  def testClassNames() = {
    testCompilePhase.classFiles.map(_.className(outputDir(TestCompilePhase))).filterNot(_.contains("$")).filter(isAccessibleScalaTestSuite).toList
  }

  def constructorCodeAsString : String = throw new Exception("Only supported by test projects")

  /********************
  *     Paths and files
  ********************/

  def makerDirectory = mkdirs(rootAbsoluteFile, ".maker")
  def cacheDirectory = mkdirs(makerDirectory, "cache")

  def managedJars = findJars(managedLibDir)
  def classpathJars : Iterable[File] = findJars(unmanagedLibDirs.toSet + managedLibDir).toSet + props.ProjectScalaLibraryJar() + props.ProjectScalaCompilerJar() + props.ProjectScalaReflectJar()

  def publishLocalJarDir = file(publishLocalDir, "/jars/").makeDir
  def publishLocalJar = file(publishLocalJarDir, packageJar(SourceCompilePhase).getName)
  def publishLocalSourceJar = file(publishLocalJarDir, sourcePackageJar(SourceCompilePhase).getName)

  def sourceDirs(compilePhase : CompilePhase) : List[File] = compilePhase match {
    case SourceCompilePhase => 
      List(file(rootAbsoluteFile, "src/main/scala"), file(rootAbsoluteFile, "src/main/java"))
    case TestCompilePhase => 
      List(file(rootAbsoluteFile, "src/test/scala"), file(rootAbsoluteFile, "src/test/java"))
  }

  def resourceDir(compilePhase : CompilePhase) = compilePhase match {
    case SourceCompilePhase => file(rootAbsoluteFile, "src/main/resources")
    case TestCompilePhase => file(rootAbsoluteFile, "src/test/resources")
  }

  def targetDir = file(rootAbsoluteFile, "target-maker")
  def outputDir(compilePhase : CompilePhase) = compilePhase match {
    case SourceCompilePhase => file(targetDir, "classes")
    case TestCompilePhase => file(targetDir, "test-classes")
  }
  override def pack = executeWithDependencies(PackageJarTask(_, SourceCompilePhase, includeUpstreamModules = false))
  def packAll = executeWithDependencies(PackageJarTask(_, SourceCompilePhase, includeUpstreamModules = true))

  def doc = executeWithDependencies(DocTask(this))
  def docOutputDir = file(targetDir, "docs")
  def packageDir = file(targetDir, "package")
  def managedLibDir = file(rootAbsoluteFile, "lib_managed")
  def managedLibSourceDir = file(rootAbsoluteFile, "lib_src_managed")
  def managedResourceDir = file(rootAbsoluteFile, "resource_managed")
  def unmanagedLibDirs : Iterable[File] = List(file(rootAbsoluteFile, "lib"))
  def warnUnnecessaryResources = true
}

trait ClassicLayout {
  this: Module =>
  override def sourceDirs(compilePhase : CompilePhase) : List[File] = compilePhase match {
    case SourceCompilePhase => file(rootAbsoluteFile, "src") :: Nil
    case TestCompilePhase => file(rootAbsoluteFile, "tests") :: Nil
  }
  override def resourceDir(compilePhase : CompilePhase) = compilePhase match {
    case SourceCompilePhase => file(rootAbsoluteFile, "resources")
    case TestCompilePhase => file(rootAbsoluteFile, "test-resources")
  }
}


object Module{
 
  val logger = LoggerFactory.getLogger(this.getClass)
  val props = MakerProps()

  val analyses = new ConcurrentHashMap[File, Analysis]()

  import maker.utils.RichIterable._
  import maker.utils.FileUtils._
  def asClasspathStr(files : Iterable[File], sep : String = java.io.File.pathSeparator) =
    files.distinctBy { (f1, f2) =>
      // distinction rules remove dupe jars (logback really doesn't like dupes)
      (f1.getAbsolutePath == f2.getAbsolutePath) || (
        f1.isFile && f2.isFile &&
          f1.getName == f2.getName &&
          f1.getName.endsWith(".jar") &&
          f1.length() == f2.length()
      )
    }.map(_.getAbsolutePath).sortWith(_ < _).mkString(sep)

  def warnOfUnnecessaryDependencies(proj : Module){

    proj.immediateUpstreamModules.foreach{
      p => 
        proj.immediateUpstreamModules.filterNot(_ == p).find(_.allUpstreamModules.contains(p)).foreach{
          p1 => 
            logger.warn("Module " + proj.name + " doesn't need to depend on " + p.name + " as it is already inherited from " + p1.name)
        }
    }


    proj.immediateUpstreamTestModules.foreach{
      p => 
        proj.immediateUpstreamTestModules.filterNot(_ == p).find(_.allUpstreamTestModules.contains(p)).foreach{
          p1 => 
            logger.warn("Module " + proj.name + " doesn't need a test dependency on " + p.name + " as it is already inherited from " + p1.name)
        }
    }

    val jarNames = proj.managedJars.map(_.getName).toSet
    proj.immediateUpstreamModules.foreach{
      upstreamModule =>
        val upstreamJarNames = upstreamModule.allUpstreamModules.flatMap(_.managedJars).map(_.getName).toSet
        val redundantJarNames = upstreamJarNames intersect jarNames
        if (redundantJarNames.nonEmpty && proj.warnUnnecessaryResources)
          logger.warn("Module " + proj.name + " doesn't need jars " + redundantJarNames.mkString(", ") + " as they are supplied by " + upstreamModule.name)
    }

  }

}

case class InvalidModuleException(msg : String) extends RuntimeException(msg)
