package maker.project

import maker.task._
import maker._
import maker.task.compile._
import java.io._
import maker.utils._
import maker.utils.FileUtils._
import maker.task.tasks._
import maker.utils.RichString._
import java.net.URLClassLoader
import java.lang.reflect.Modifier
import scala.xml.{Elem, NodeSeq}
import java.util.concurrent.atomic.{AtomicReference, AtomicBoolean}
import sbt.inc.Analysis
import java.util.concurrent.ConcurrentHashMap

trait ProjectTrait extends MakerConfig with ScalaJars with Log {
  protected def root : File
  /**
   * The standard equals and hashCode methods were slow, making Dependency operations very expensive.
   */
   override def equals(rhs : Any) = {
     (this, rhs) match {
       case (l : Module, r : Module) if l.root == r.root => {
         //I believe this assertion should always hold. It's really here so that
         //this overriden equals method never returns true on differing modules.
         assert(this eq r, "Shouldn't have two modules pointing to the same root")
         true
       }
       case (l : Project, r : Project) if l.root == r.root => {
         //I believe this assertion should always hold. It's really here so that
         //this overriden equals method never returns true on differing modules.
         assert(this eq r, "Shouldn't have two projects pointing to the same root")
         true
       }
       case _ => false
     }
   }
  override def hashCode = root.hashCode

  def extraTestSystemProperties: Seq[String]
  def extraJars: Seq[File]
  def scalaVersion : ScalaVersion 

  val analyses = new ConcurrentHashMap[File, Analysis]()

  val rootAbsoluteFile = root.asAbsoluteFile
  def name : String
  def extraUpstreamTasks(task: Task): Seq[Task] = Nil
  def extraDownstreamTasks(task: Task): Seq[Task] = Nil

  def resourceCacheDirectory: File = {
    mkdirs(file(System.getProperty("user.home"), ".maker", "resource-cache"))
  }

  private val dependenciesUpdated = new AtomicBoolean(false)
  def markDependenciesUpdated() = {
    dependenciesUpdated.set(true)
  }
  def dependenciesAlreadyUpdated() = dependenciesUpdated.get
  def clearDependencies() = {
    dependenciesUpdated.set(false)

    cleanRegularFilesLeavingDirectories(managedLibDir)
    cleanRegularFilesLeavingDirectories(managedLibSourceDir)
    cleanRegularFilesLeavingDirectories(testManagedLibDir)
    cleanRegularFilesLeavingDirectories(testManagedLibSourceDir)
  }

  protected def transitiveClosure[A](start : Seq[A], expand : A => Seq[A]) : Seq[A] = {
    var closure : Seq[A] = Nil
    var more = start.distinct
    while (more.nonEmpty){
      closure ++= more
      more = more.flatMap(expand).distinct.filterNot(closure.contains)
    }
    closure.distinct
  }

  // Note that 'upstream' is inclusive of `this`, when `this` is a module
  def upstreamModules : Seq[Module]
  def testUpstreamModules : Seq[Module]

  def setUp(graph : Dependency.Graph) : Boolean = {
    if (graph.includesCompileTask){
      CompileTask.topLevelCompilationErrorsFile.delete
    }
    ! CompileTask.topLevelCompilationErrorsFile.exists()
  }
  protected def tearDown(graph : Dependency.Graph, result : BuildResult) : Boolean = true

  protected def transitiveBuild(rootTasks : Seq[Task]) = {
    Build(
      rootTasks.headOption.map(_.name).getOrElse("Empty build"),
      Dependency.Graph.transitiveClosure(this, rootTasks),
      taskThreadPoolSize
    )
  }


  def compileTaskBuild(phases: Seq[CompilePhase]): Build 
  def compile : BuildResult = execute(compileTaskBuild(SourceCompilePhase :: Nil))


  //def testCompileTaskBuild(testPhases : Seq[CompilePhase]) : Build 
  def testCompile : BuildResult = execute(compileTaskBuild(CompilePhase.TEST_PHASES))

  def tcc = Continuously(this, () => testCompile)
  
  def testFailedSuites : BuildResult = execute(
    transitiveBuild(upstreamModules.map(RunUnitTestsTask.failingTests(this, _)))
  )

  def update: BuildResult = execute(transitiveBuild(UpdateTask(this) :: Nil))

  def forceUpdate : BuildResult = {
    clearDependencies()
    update
  }


  def runMain(className : String)(opts : String*)(args : String*) = 
    execute(
      transitiveBuild(RunMainTask(this, className, opts, args, scalaVersion) :: Nil)
    )

  def clean = execute(transitiveBuild(CleanTask(this) :: Nil))

  protected def execute(bld : Build) = {
    setUp(bld.graph)
    val result = bld.execute
    tearDown(bld.graph, result)
    if (result.failed && runningInExecMode && systemExitOnExecModeFailures){
      logger.error(bld + " failed - exiting")
      System.exit(1)
    }
    BuildResult.lastResult.set(Some(result))
    if(reportBuildResult)
      result.reportResult
    result
  }

  def compilationTargetDirectories(phases : Seq[CompilePhase]) = {
    val phases_ = (SourceCompilePhase +: phases).distinct
    phases_.flatMap{
      phase => 
        phase match {
          case SourceCompilePhase => 
            upstreamModules.map(_.classDirectory(phase))
          case _ => 
            testUpstreamModules.map(_.classDirectory(phase)) ++: 
            testUpstreamModules.map(_.classDirectory(SourceCompilePhase)) 
        }
    }.distinct
  }

  private [project] def resourceDirectories(phases : Seq[CompilePhase]) = upstreamModules.flatMap{
    module => 
      // Put test resources first so that test config files override prod
      val (testPhases, otherPhases) = phases.partition(_.isInstanceOf[TestPhase])
      (testPhases ++: otherPhases).map(module.resourceDir)
  }

  private [project] def dependencyJars(phase : CompilePhase) = phase match {
    case SourceCompilePhase => 
      findJars(managedLibDir)
    case _ => 
      findJars(testManagedLibDir)
  }
  private [project] def unmanagedLibs = findJars(upstreamModules.flatMap(_.unmanagedLibDirs))

  def compilationClasspathComponents(phase : CompilePhase) = {
    val compileTasks : Seq[CompileTask] = compileTaskBuild(phase :: Nil).tasks.toSeq.flatMap {
      case t : CompileTask => Some(t)
      case _ =>  None
    }
    compileTasks.map {
      case CompileTask(_, module, phase) => 
        module.classDirectory(phase)
    } ++:
    dependencyJars(phase) ++:
    unmanagedLibs
  }

  def compilationClasspath(phase : CompilePhase) = {
    Module.asClasspathStr(compilationClasspathComponents(phase))
  }


  def runtimeClasspathComponents(phases : Seq[CompilePhase]): Seq[File] = {
    if (phases.contains(SourceCompilePhase)) {
      val components = extraJars ++:
        compilationTargetDirectories(phases) ++:
        (if (phases.toSet.intersect(CompilePhase.TEST_PHASES.toSet).nonEmpty)
          dependencyJars(TestCompilePhase) 
        else
          dependencyJars(SourceCompilePhase)
        ) ++:
        unmanagedLibs ++:
        resourceDirectories(phases) 
      components.distinct
    } else {
      runtimeClasspathComponents(SourceCompilePhase +: phases)
    }
  }

  def runtimeClasspath(phases : Seq[CompilePhase]) = Module.asClasspathStr(runtimeClasspathComponents(phases))



  def testResults: MakerTestResults 


  def managedLibDir = file(rootAbsoluteFile, "lib_managed", scalaVersion.versionNo)
  def testManagedLibDir = file(rootAbsoluteFile, "test_lib_managed", scalaVersion.versionNo)
  def managedLibSourceDir = file(rootAbsoluteFile, "lib_src_managed", scalaVersion.versionNo)
  def testManagedLibSourceDir = file(rootAbsoluteFile, "test_lib_src_managed", scalaVersion.versionNo)
  def managedResourceDir = file(rootAbsoluteFile, "resource_managed")
  def unmanagedLibDirs : Seq[File] = List(file(rootAbsoluteFile, "lib"))

  def upstreamDependencies = (upstreamModules ++ testUpstreamModules.flatMap(_.testDependencies)).distinct.flatMap(_.dependencies)

  def testClasspathLoader = new URLClassLoader(
    runtimeClasspathComponents(CompilePhase.TEST_PHASES).map(_.toURI.toURL).toArray,
    null
  )
  def testTaskBuild(testPhase: TestPhase, lastCompilationTimeFilter : Option[Long]) : Build

  def test : BuildResult = {
    execute(testTaskBuild(TestCompilePhase, lastCompilationTimeFilter = None))
  }

  def integrationTest: BuildResult = {
    execute(testTaskBuild(IntegrationTestCompilePhase, lastCompilationTimeFilter = None))
  }

  def endToEndTest: BuildResult = {
    execute(testTaskBuild(EndToEndTestCompilePhase, lastCompilationTimeFilter = None))
  }
  def testQuick : BuildResult = {
    execute(testTaskBuild(TestCompilePhase, lastCompilationTimeFilter = Some(System.currentTimeMillis)))
  }


  def testQuickContinuously = Continuously(this, () => testQuick)
  def tqc = testQuickContinuously

}
