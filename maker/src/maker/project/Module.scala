package maker.project

import java.io.File
import java.util.concurrent.ConcurrentHashMap
import maker.task._
import maker.task.compile._
import maker.task.tasks._
import maker.utils.FileUtils._
import maker.PomUtils
import org.slf4j.LoggerFactory
import sbt.ConsoleLogger
import sbt.inc.Analysis
import scala.collection.immutable.Nil
import com.typesafe.config.{ConfigFactory, Config}
import org.eclipse.aether.graph.{Exclusion, Dependency => AetherDependency}
import maker.utils.FileUtils

/**
  * Corresponds to a module in IntelliJ
  */

class Module(
    val root : File,
    val name : String,
    val config : Config = ConfigFactory.load(),
    val immediateUpstreamModules : Seq[Module] = Nil,
    val testModuleDependencies : Seq[Module] = Nil,
    val analyses : ConcurrentHashMap[File, Analysis] = Module.analyses
)
  extends ProjectTrait
  with DependencyPimps
{

  import Module.logger
  def modules = this :: Nil
  protected val upstreamModulesForBuild = List(this)

  def tearDown(graph : Dependency.Graph, result : BuildResult) = true
  def dependencies : Seq[RichDependency]  = Nil

  def compilationMetadataDirectory(majorScalaVersion : String, phase : CompilePhase) = 
    mkdir(file(makerDirectory, "compilation-metadata", majorScalaVersion, phase.name))
  def compilationCacheFile(majorScalaVersion : String, phase : CompilePhase) = {
    file(compilationMetadataDirectory(majorScalaVersion, phase), "compilation-analysis-cache")
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
        otherUpstreamModules.find(_.upstreamModules.contains(module)) match {
          case Some(otherUpstreamModule) =>
          logger.warn(name + " shouldn't depend on " + module.name + " as it is inherited via " + otherUpstreamModule.name)
          case None =>
        }
    }
  }

  warnOfRedundantDependencies()

  def testCompilePhase = ModuleCompilePhase(this, TestCompilePhase)
  def compilePhase = ModuleCompilePhase(this, SourceCompilePhase)


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
      config.taskThreadPoolSize
    )
    execute(build)
  }

  def cleanOnly = executeSansDependencies(CleanTask(this, defaultMajorScalaVersion))

  def testTaskBuild(majorScalaVersion : String) = {
    // For a module, the `test` task runs just tha module's tests.
    // To run all tests, use the containing project
    transitiveBuild(
      RunUnitTestsTask(
        s"Unit tests for $this", 
        modules = this :: Nil, 
        rootProject = this, 
        classOrSuiteNames_ = None,
        majorScalaVersion = majorScalaVersion
      ) :: Nil
    )
  }


  def test(majorScalaVersion : String) : BuildResult = execute(testTaskBuild(majorScalaVersion))
  def test : BuildResult = test(defaultMajorScalaVersion)

  def testCompileTaskBuild(majorScalaVersion : String) = transitiveBuild(
    (this +: testModuleDependencies).map(TestCompileTask(this, _, majorScalaVersion))
  )

  def testFailuredSuitesOnly(majorScalaVersion : String) : BuildResult = executeSansDependencies(
    RunUnitTestsTask.failingTests(this, this, majorScalaVersion)
  )
  def testFailuredSuitesOnly : BuildResult = testFailuredSuitesOnly(defaultMajorScalaVersion)


  /********************
  *     Test classses 
  ********************/

  def classFiles(majorScalaVersion : String) : Seq[File] = FileUtils.findClasses(classDirectory(majorScalaVersion))
  def testClassFiles(majorScalaVersion : String) : Seq[File] = FileUtils.findClasses(testClassDirectory(majorScalaVersion))
  def classFiles(majorScalaVersion : String, phase : CompilePhase) : Seq[File] = phase match {
    case SourceCompilePhase => classFiles(majorScalaVersion)
    case TestCompilePhase   => testClassFiles(majorScalaVersion)
  }

  def testClassNames(rootProject : ProjectTrait, majorScalaVersion : String) : Seq[String] = {
    val isTestSuite = isAccessibleScalaTestSuite(rootProject, majorScalaVersion)
    testClassFiles(majorScalaVersion).map(_.className(testClassDirectory(majorScalaVersion))).filterNot(_.contains("$")).filter(isTestSuite).toList
  }


  /********************
  *     Paths and files
  ********************/

  def makerDirectory = mkdirs(rootAbsoluteFile, ".maker")
  def cacheDirectory = mkdirs(makerDirectory, "cache")


  def sourceDirs(compilePhase : CompilePhase) : List[File] = compilePhase match {
    case SourceCompilePhase => 
      List(file(rootAbsoluteFile, "src/main/scala"), file(rootAbsoluteFile, "src/main/java"))
    case TestCompilePhase => 
      List(file(rootAbsoluteFile, "src/test/scala"), file(rootAbsoluteFile, "src/test/java"))
  }

  def scalaFiles(phase : CompilePhase) = findFilesWithExtension("scala", sourceDirs(phase) : _*)
  def javaFiles(phase : CompilePhase) = findFilesWithExtension("java", sourceDirs(phase): _*)

  def sourceFiles(phase : CompilePhase) = scalaFiles(phase) ++ javaFiles(phase)

  def resourceDir(compilePhase : CompilePhase) = compilePhase match {
    case SourceCompilePhase => file(rootAbsoluteFile, "src/main/resources")
    case TestCompilePhase => file(rootAbsoluteFile, "src/test/resources")
  }

  def targetDir = file(rootAbsoluteFile, "target-maker")
  def classDirectory(majorScalaVersion : String) = file(targetDir, majorScalaVersion, "classes")
  def testClassDirectory(majorScalaVersion : String) = file(targetDir, majorScalaVersion, "test-classes")
  def classDirectory(majorScalaVersion : String, phase : CompilePhase) : File = {
    phase match {
      case SourceCompilePhase => classDirectory(majorScalaVersion)
      case TestCompilePhase   => testClassDirectory(majorScalaVersion)
    }
  }
  //def outputDir(compilePhase : CompilePhase) = compilePhase match {
    //case SourceCompilePhase => file(targetDir, "classes")
    //case TestCompilePhase => file(targetDir, "test-classes")
  //}

  def warnUnnecessaryResources = true
  def vimModuleCompileOutputFile = file(root, "vim-compile-output")

  def compilerName = "zinc"

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
 
  lazy val logger = LoggerFactory.getLogger(this.getClass)

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
        proj.immediateUpstreamModules.filterNot(_ == p).find(_.upstreamModules.contains(p)).foreach{
          p1 => 
            logger.warn("Module " + proj.name + " doesn't need to depend on " + p.name + " as it is already inherited from " + p1.name)
        }
    }


    val strictlyUpstreamDependencies = (proj.immediateUpstreamModules ++ proj.testModuleDependencies).distinct.map{
      module => 
        module -> module.upstreamDependencies
    }.toMap

    proj.dependencies.foreach{
      dependency => 
        strictlyUpstreamDependencies.find{
          case (_, upstreamDeps) => upstreamDeps.contains(dependency)
        } match {
          case Some((upstreamModule, _)) => 
            logger.warn("Module " + proj.name + " doesn't need dependency " + dependency + " as it is supplied by " + upstreamModule.name)
          case None => 
        }
    }


  }

}

case class InvalidModuleException(msg : String) extends RuntimeException(msg)
