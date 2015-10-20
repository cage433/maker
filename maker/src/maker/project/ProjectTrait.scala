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

  def scalaVersion : ScalaVersion 

  val rootAbsoluteFile = root.asAbsoluteFile
  protected def name : String
  protected def furthestDownstreamModules : Seq[Module]
  def extraUpstreamTasks(task: Task): Seq[Task] = Nil
  def extraDownstreamTasks(task: Task): Seq[Task] = Nil



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
    var more = start
    while (more.nonEmpty){
      closure ++= more
      more = more.flatMap(expand).filterNot(closure.contains)
    }
    closure
  }
  // Note that 'upstream' is inclusive of `this`, when `this` is a module
  def upstreamModules : Seq[Module] = transitiveClosure(furthestDownstreamModules, {m : Module => m.immediateUpstreamModules})

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


  def compileTaskBuild = transitiveBuild(furthestDownstreamModules.map(CompileTask(this, _, SourceCompilePhase)))
  def compile : BuildResult = execute(compileTaskBuild)


  def testCompileTaskBuild(testPhases : Seq[CompilePhase]) : Build 
  def testCompile : BuildResult = execute(testCompileTaskBuild(CompilePhase.TEST_PHASES))
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

  private [project] def compilationTargetDirectories(phases : Seq[CompilePhase]) = {
    val phases_ = (SourceCompilePhase +: phases).distinct
    phases_.flatMap{
      phase => 
        phase match {
          case SourceCompilePhase => 
            upstreamModules.map(_.classDirectory(phase))
          case _ => 
            (this match {
              case _ : Project => 
                upstreamModules.map(_.classDirectory(phase))
              case m : Module => 
                (m +: upstreamModules.flatMap(_.testModuleDependencies)).map(_.classDirectory(phase))
            })
        }
    }
  }

  private [project] def resourceDirectories(phases : Seq[CompilePhase]) = upstreamModules.flatMap{
    module => 
      phases.map(module.resourceDir(_))
  }

  private [project] def dependencyJars(phase : CompilePhase) = phase match {
    case SourceCompilePhase => 
      findJars(managedLibDir)
    case _ => 
      findJars(testManagedLibDir)
  }
  private [project] def unmanagedLibs = findJars(upstreamModules.flatMap(_.unmanagedLibDirs))

  def compilationClasspathComponents(phase : CompilePhase) = 
      compilationTargetDirectories(phase :: Nil) ++:
      dependencyJars(phase) ++:
      unmanagedLibs

  def compilationClasspath(phase : CompilePhase) = 
    Module.asClasspathStr(compilationClasspathComponents(phase))


  def runtimeClasspathComponents(phases : Seq[CompilePhase]) = 
    compilationTargetDirectories(phases) ++:
    (if (phases.toSet.intersect(CompilePhase.TEST_PHASES.toSet).nonEmpty)
      dependencyJars(TestCompilePhase) 
    else
      dependencyJars(SourceCompilePhase)
    ) ++:
    unmanagedLibs ++:
    resourceDirectories(phases) 

  def runtimeClasspath(phases : Seq[CompilePhase]) = Module.asClasspathStr(runtimeClasspathComponents(phases))


  def testResults = {
    // Test results may either be in a top level project's directory, or else in
    // module directoriy(s)
    upstreamModules.distinct.map(MakerTestResults(_)).reduce(_++_)
  }


  def constructorCodeAsString : String = throw new Exception("Only supported by test projects")

  def managedLibDir = file(rootAbsoluteFile, "lib_managed", scalaVersion.versionNo)
  def testManagedLibDir = file(rootAbsoluteFile, "test_lib_managed", scalaVersion.versionNo)
  def managedLibSourceDir = file(rootAbsoluteFile, "lib_src_managed", scalaVersion.versionNo)
  def testManagedLibSourceDir = file(rootAbsoluteFile, "test_lib_src_managed", scalaVersion.versionNo)
  def managedResourceDir = file(rootAbsoluteFile, "resource_managed")
  def unmanagedLibDirs : Seq[File] = List(file(rootAbsoluteFile, "lib"))

  def upstreamDependencies = (upstreamModules ++ upstreamModules.flatMap(_.testModuleDependencies)).distinct.flatMap(_.dependencies)

  def testClasspathLoader(testPhase : CompilePhase) = new URLClassLoader(
    runtimeClasspathComponents(testPhase :: Nil).map(_.toURI.toURL).toArray,
    null
  )
  def testTaskBuild(lastCompilationTimeFilter : Option[Long]) : Build

  def test : BuildResult = {
    execute(testTaskBuild(lastCompilationTimeFilter = None))
  }


  def testQuick : BuildResult = {
    execute(testTaskBuild(lastCompilationTimeFilter = Some(System.currentTimeMillis)))
  }


  def testQuickContinuously = Continuously(this, () => testQuick)
  def tqc = testQuickContinuously

}
