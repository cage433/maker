package maker.project

import java.io.{File, FileOutputStream}
import java.util.concurrent.ConcurrentHashMap
import maker.task._
import maker.task.compile._
import maker.task.tasks._
import maker.utils.FileUtils._
import maker.{PomUtils, ScalaVersion, Log}
import sbt.ConsoleLogger
import sbt.inc.Analysis
import scala.collection.immutable.Nil
import org.eclipse.aether.graph.{Exclusion, Dependency => AetherDependency}
import maker.utils.{FileUtils, MakerTestResults}
import org.apache.commons.io.output.TeeOutputStream

/**
  * Corresponds to a module in IntelliJ
  */
 case class Module(
    root : File,
    name : String,
    compileDependencies : Seq[Module] = Nil,
    testDependencies : Seq[Module] = Nil,
    scalaVersion: ScalaVersion = ScalaVersion.TWO_TWELVE_DEFAULT,
    extraJars: Seq[File] = Nil,                                   /* Initital use case is to add jfxrt.jar */
    extraTestSystemProperties: Seq[String] = Nil

)
  extends ProjectTrait
  with DependencyPimps
  with Log
{

  import Module.logger
  def upstreamModules : Seq[Module] = transitiveClosure(Seq(this), {m : Module => m.compileDependencies})
  def testUpstreamModules : Seq[Module] = {
    val testDependencyClosure = transitiveClosure(Seq(this), {m : Module => m.testDependencies})
    transitiveClosure(this +: testDependencyClosure, {m : Module => m.compileDependencies}).distinct
  }

  override def tearDown(graph : Dependency.Graph, result : BuildResult) = true

  def dependencies : Seq[RichDependency]  = Nil

  def compilationMetadataDirectory(phase : CompilePhase) = 
    mkdirs(file(makerDirectory, "compilation-metadata", scalaVersion.versionBase, phase.name))

  def compilationCacheFile(phase : CompilePhase) = {
    file(compilationMetadataDirectory(phase), "compilation-analysis-cache")
  }

  def lastCompilationTime(phase : CompilePhase) : Option[Long] = {
    if (compilationCacheFile(phase).exists)
      lastModifiedProperFileTime(Vector(compilationCacheFile(phase)))
    else
      None
  }

  def lastSourceModifcationTime(phase : CompilePhase) : Option[Long] = lastModifiedProperFileTime(sourceFiles(phase))

  def moduleCompilationErrorsFile(phase : CompilePhase) = {
    file(compilationMetadataDirectory(phase), "vim-compile-errors")
  }

  def compilationOutputStream(phase : CompilePhase) = {
    new TeeOutputStream(
      Console.err,
      new FileOutputStream(moduleCompilationErrorsFile(phase))
    )
  }
  def compilationFailedMarker(phase : CompilePhase) = 
    file(compilationMetadataDirectory(phase), "compilation-failed-marker")

  def lastCompilationFailed(phase : CompilePhase) = 
    compilationFailedMarker(phase).exists

  def markCompilatonFailure(phase : CompilePhase) = compilationFailedMarker(phase).touch

  Module.warnOfUnnecessaryDependencies(this)

  def javacOptions : Seq[String] = Nil 
  def scalacOptions : Seq[String] = "-feature" :: Nil
  /**
   * The standard equals method was slow, making Dependency operations very expensive.
   */
   override def equals(rhs : Any) = {
     rhs match {
       case p : Module if p.root == root => {
         //I believe this assertion should always hold. It's really here so that
         //this overriden equals method never returns true on differing modules.
         assert(this eq p, s"Shouldn't have two modules pointing to the same root - have $this and $rhs")
         true
       }
       case _ => false
     }
   }

  override def hashCode = root.hashCode

  private def warnOfRedundantDependencies() {
    compileDependencies.foreach{
      module =>
        val otherUpstreamModules = compileDependencies.filterNot(_ == module)
        otherUpstreamModules.find(_.upstreamModules.contains(module)) match {
          case Some(otherUpstreamModule) =>
          logger.warn(name + " shouldn't depend on " + module.name + " as it is inherited via " + otherUpstreamModule.name)
          case None =>
        }
    }
  }

  warnOfRedundantDependencies()


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
      taskThreadPoolSize
    )
    execute(build)
  }

  def cleanOnly = executeSansDependencies(CleanTask(this))

  def testClass(className: String): BuildResult = execute(
    transitiveBuild(
      RunUnitTestsTask(
        s"Testing $className",
        this,
        this,
        Left(className :: Nil),
        lastCompilationTimeFilter = None
      ) :: Nil
    )
  )

  def testTaskBuild(testPhase: TestPhase, lastCompilationTimeFilter : Option[Long]) = {
    // For a module, the `test` task runs just that module's tests.
    // To run all tests, use the containing project
    transitiveBuild(
      RunUnitTestsTask(
        s"Unit tests for $this", 
        module = this, 
        rootProject = this, 
        classNamesOrPhase = Right(testPhase),
        lastCompilationTimeFilter = lastCompilationTimeFilter
      ) :: Nil
    )
  }

  def testResults = MakerTestResults(this)

  def compileTaskBuild(phases: Seq[CompilePhase]): Build = transitiveBuild(phases.map(CompileTask(this, this, _)))


  //def testCompileTaskBuild(testPhases : Seq[CompilePhase]) = transitiveBuild(
    //(this +: testDependencies).flatMap{module => 
      //testPhases.map(CompileTask(this, module, _))
    //}
  //)

  def testFailuredSuitesOnly : BuildResult = executeSansDependencies(
    RunUnitTestsTask.failingTests(this, this)
  )


  /********************
  *     Test classses 
  ********************/

  def classFiles(phase : CompilePhase) : Seq[File] = FileUtils.findClasses(classDirectory(phase))

  /********************
  *     Paths and files
  ********************/

  def makerDirectory = mkdirs(rootAbsoluteFile, ".maker")


  def sourceDirs(compilePhase : CompilePhase) : List[File] = compilePhase match {
    case SourceCompilePhase => 
      List(file(rootAbsoluteFile, "src/main/scala"), file(rootAbsoluteFile, "src/main/java"))
    case TestCompilePhase => 
      List(file(rootAbsoluteFile, "src/test/scala"), file(rootAbsoluteFile, "src/test/java"))
    case IntegrationTestCompilePhase => 
      List(file(rootAbsoluteFile, "src/it/scala"), file(rootAbsoluteFile, "src/it/java"))
    case EndToEndTestCompilePhase => 
      List(file(rootAbsoluteFile, "src/e2e/scala"), file(rootAbsoluteFile, "src/e2e/java"))
  }

  def scalaFiles(phase : CompilePhase) = findFilesWithExtension("scala", sourceDirs(phase) : _*)
  def javaFiles(phase : CompilePhase) = findFilesWithExtension("java", sourceDirs(phase): _*)

  def sourceFiles(phase : CompilePhase) = scalaFiles(phase) ++ javaFiles(phase)

  def resourceDir(compilePhase : CompilePhase) = compilePhase match {
    case SourceCompilePhase => file(rootAbsoluteFile, "src/main/resources")
    case TestCompilePhase => file(rootAbsoluteFile, "src/test/resources")
    case IntegrationTestCompilePhase => file(rootAbsoluteFile, "src/it/resources")
    case EndToEndTestCompilePhase => file(rootAbsoluteFile, "src/e2e/resources")
  }

  def targetDir = file(rootAbsoluteFile, "target-maker")
  def classDirectory(phase : CompilePhase) : File = {
    phase match {
      case SourceCompilePhase             => file(targetDir, scalaVersion.versionNo, "classes")
      case TestCompilePhase               => file(targetDir, scalaVersion.versionNo, "test-classes")
      case IntegrationTestCompilePhase    => file(targetDir, scalaVersion.versionNo, "integration-test-classes")
      case EndToEndTestCompilePhase       => file(targetDir, scalaVersion.versionNo, "end-to-end-test-classes")
    }
  }

  def warnUnnecessaryResources = true
  def vimModuleCompileOutputFile = file(root, "vim-compile-output")

  def compilerName = "zinc"
}


object Module extends Log {
 

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

    proj.compileDependencies.foreach{
      p => 
        proj.compileDependencies.filterNot(_ == p).find(_.upstreamModules.contains(p)).foreach{
          p1 => 
            logger.warn("Module " + proj.name + " doesn't need to depend on " + p.name + " as it is already inherited from " + p1.name)
        }
    }


    val strictlyUpstreamDependencies = (proj.compileDependencies ++ proj.testDependencies).distinct.map{
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
