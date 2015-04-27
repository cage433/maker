package maker.project

import maker.task._
import maker._
import maker.task.compile._
import java.io.{File, FileOutputStream}
import maker.utils._
import maker.utils.FileUtils._
import maker.task.tasks._
import maker.utils.RichString._
import java.net.URLClassLoader
import java.lang.reflect.Modifier
import scala.xml.{Elem, NodeSeq}
import org.slf4j.LoggerFactory
import scala.collection.immutable.Nil
import com.typesafe.config.Config
import org.apache.commons.io.output.TeeOutputStream
import java.util.concurrent.atomic.AtomicReference

trait ProjectTrait extends ConfigPimps{
  protected def root : File
  /**
   * The standard equals method was slow, making Dependency operations very expensive.
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
  val rootAbsoluteFile = root.asAbsoluteFile
  lazy val testOutputFile = file(rootAbsoluteFile, "maker-test-output")
  def name : String
  def modules : Seq[Module]
  def testModuleDependencies : Seq[Module]
  def config : Config
  def extraUpstreamTasksMatcher : PartialFunction[Task, Set[Task]] = Map.empty
  def extraDownstreamTasksMatcher : PartialFunction[Task, Set[Task]] = Map.empty

  def topLevelCompilationErrorsFile = file("vim-compilation-errors")

  def runMainOutputStream = {
    val runLogFile = file(rootAbsoluteFile, "runlog.out")
    new TeeToFileOutputStream(runLogFile)
  }

  def compilationOutputStream(phase : CompilePhase) = {
    val moduleCompilationErrorsFile = phase match {
      case TestCompilePhase =>  file(rootAbsoluteFile, "module-vim-test-compile-errors")
      case SourceCompilePhase => file(rootAbsoluteFile, "module-vim-compile-errors")
    }
    new TeeOutputStream(
      Console.err,
      new FileOutputStream(moduleCompilationErrorsFile)
    )
  }

  private val scalatestOutputParameters_ = new AtomicReference[String]("-oHL")
  def scalatestOutputParameters : String = scalatestOutputParameters_.get

  def toggleTestExceptions {
    if (scalatestOutputParameters == "-oHL")
      scalatestOutputParameters_.set("-oFHL")
    else
      scalatestOutputParameters_.set("-oHL")
  }

  def reportBuildResult : Boolean = true

  def systemExitOnExecModeFailures : Boolean = true

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
  def upstreamModules : Seq[Module] = transitiveClosure(modules, {m : Module => m.immediateUpstreamModules})

  def setUp(graph : Dependency.Graph) : Boolean = {
    if (graph.includesCompileTask){
      topLevelCompilationErrorsFile.delete
    }
    ! topLevelCompilationErrorsFile.exists()
  }
  def tearDown(graph : Dependency.Graph, result : BuildResult) : Boolean

  protected def transitiveBuild(rootTasks : Seq[Task]) = {
    Build(
      rootTasks.headOption.map(_.name).getOrElse("Empty build"),
      Dependency.Graph.transitiveClosure(rootTasks, extraUpstreamTasksMatcher, extraDownstreamTasksMatcher),
      config.taskThreadPoolSize
    )
  }


  def compileTaskBuild() = transitiveBuild(modules.map(SourceCompileTask(this, _)))
  def compile = execute(compileTaskBuild())


  def testCompileTaskBuild : Build // = transitiveBuild(upstreamModules.map(SourceCompileTask(this, _)) ++ upstreamTestModules.map(TestCompileTask(this, _)))
  def testCompile = execute(testCompileTaskBuild)
  def tcc = continuously(testCompileTaskBuild)
  
  def testFailedSuitesBuild = {
    transitiveBuild(upstreamModules.map(RunUnitTestsTask.failingTests(this, _)))
  }
  def testFailedSuites : BuildResult = execute(testFailedSuitesBuild)

  def updateTaskBuild(forceSourceUpdate : Boolean) = {
    transitiveBuild(UpdateTask(this, forceSourceUpdate = forceSourceUpdate) :: Nil)
  }
  def update = execute(updateTaskBuild(forceSourceUpdate = false))
  def updateSources = execute(updateTaskBuild(forceSourceUpdate = true))


  def runMainTaskBuild(className : String, opts : Seq[String], args : Seq[String]) = {
    transitiveBuild(RunMainTask(this, className, opts, args) :: Nil)
  }
  def runMain(className : String)(opts : String*)(args : String*) = 
    execute(runMainTaskBuild(className, opts, args))

  def clean = execute(transitiveBuild(CleanTask(this) :: Nil))
  protected def execute(bld : Build) = {
    setUp(bld.graph)
    val result = bld.execute
    tearDown(bld.graph, result)
    if (result.failed && config.execMode && systemExitOnExecModeFailures){
      logger.error(bld + " failed - exiting")
      System.exit(1)
    }
    BuildResult.lastResult.set(Some(result))
    if(reportBuildResult)
      result.reportResult
    result
  }

  private [project] def compilationTargetDirectories = upstreamModules.map(_.outputDir(SourceCompilePhase))
  private [project] def resourceDirectories = upstreamModules.map(_.resourceDir(SourceCompilePhase))
  private [project] def testResourceDirectories = upstreamModules.map(_.resourceDir(TestCompilePhase))
  private [project] def dependencyJars = findJars(managedLibDir)
  private [project] def testDependencyJars = findJars(testManagedLibDir)
  private [project] def unmanagedLibs = findJars(upstreamModules.flatMap(_.unmanagedLibDirs))

  def compilationClasspathComponents = 
      compilationTargetDirectories ++:
      dependencyJars ++:
      unmanagedLibs

  def compilationClasspath = Module.asClasspathStr(compilationClasspathComponents)

  def testCompilationClasspathComponents = 
      compilationTargetDirectories ++:
      testDependencyJars ++:
      unmanagedLibs ++:
      (this match {
        case _ : Project => 
          upstreamModules.map(_.outputDir(TestCompilePhase))
        case m : Module => 
          (m +: testModuleDependencies).map(_.outputDir(TestCompilePhase))
      })

  def testCompilationClasspath = Module.asClasspathStr(testCompilationClasspathComponents)


  def runtimeClasspathComponents = 
    compilationTargetDirectories ++:
    dependencyJars ++:
    unmanagedLibs ++:
    resourceDirectories 

  def runtimeClasspath = Module.asClasspathStr(runtimeClasspathComponents)


  def testRuntimeClasspathComponents : Seq[File] =
    compilationTargetDirectories ++:
    testDependencyJars ++:
    unmanagedLibs ++:
    resourceDirectories ++:
    (this match {
      case _ : Project => 
        upstreamModules.flatMap{m => m.outputDir(TestCompilePhase) :: m.resourceDir(TestCompilePhase) :: Nil}
      case m : Module => 
        (m +: testModuleDependencies).flatMap{m => m.outputDir(TestCompilePhase) :: m.resourceDir(TestCompilePhase) :: Nil}
    })

  def testRuntimeClasspath = Module.asClasspathStr(testRuntimeClasspathComponents)

  def continuously(bld : Build){
    var lastTaskTime :Option[Long] = None

    def allSourceFiles : Seq[File] = upstreamModules.flatMap(_.compilePhase.sourceFiles) ++: testModuleDependencies.flatMap(_.testCompilePhase.sourceFiles) 

    def sourceFileCount : Int = allSourceFiles.size
    var lastFileCount : Int = sourceFileCount 
    def sourceFileNames : String = allSourceFiles.map(_.getPath).sortWith(_<_).mkString(" ")
    var lastSourceFileNames : String = sourceFileNames

    def printWaitingMessage = println("\nWaiting for source file changes (press 'enter' to interrupt)")
    def rerunTask{
      println(execute(bld))
      lastTaskTime = Some(System.currentTimeMillis)
      lastFileCount = sourceFileCount
      lastSourceFileNames = sourceFileNames
      printWaitingMessage
    }


    def lastSrcModificationTime : Option[Long] = {
      FileUtils.lastModifiedFileTime(allSourceFiles)
      //allSourceFiles.map(FileUtils.lastModifiedFileTime(watchedFiles)).max
      //allUpstreamTestModules.map(proj => {
          //val watchedFiles = proj.compilePhase.sourceFiles ++ proj.testCompilePhase.sourceFiles
          //FileUtils.lastModifiedFileTime(watchedFiles)
        //}).max
    }
    printWaitingMessage
    while (true) {
      Thread.sleep(1000)
      if (System.in.available > 0 && System.in.read == 10) return
      (lastTaskTime,  lastSrcModificationTime, lastFileCount, sourceFileCount, lastSourceFileNames, sourceFileNames) match {
        case (None, _, _, _, _, _) => { rerunTask }                        // Task has never been run
        case (Some(t1), Some(t2), _, _, _, _) if t1 < t2 => { rerunTask }  // Code has changed since task last run
        case (_, _, m, n, _, _) if m != n => { rerunTask }                  // Source file has been added or deleted
        case (_, _, _, _, a, b) if a != b => { rerunTask }                  // Source file has been renamed
        case _ =>                                                    // Either no code yet or code has not changed
      }
    }
  }

  def isAccessibleScalaTestSuite(rootProject : ProjectTrait) : (String => Boolean) = {
    val loader = rootProject.testClasspathLoader

    className : String => {

      def loadClass(className : String) = {
        try {
          loader.loadClass(className)
        } catch {
          case e : ClassNotFoundException => 
            println(s"Couldn't load class $className in project $this, classpath was ${testRuntimeClasspathComponents.mkString("\n\t", "\n\t", "\n")}")
            throw e
        }
      }
      val suiteClass = loadClass("org.scalatest.Suite")
      val emptyClassArray = new Array[java.lang.Class[T] forSome {type T}](0)
      val clazz = loadClass(className)
      try {
        suiteClass.isAssignableFrom(clazz) &&
          Modifier.isPublic(clazz.getModifiers) &&
          !Modifier.isAbstract(clazz.getModifiers) &&
          Modifier.isPublic(clazz.getConstructor(emptyClassArray: _*).getModifiers)
      }
      catch {
        case _: NoSuchMethodException => false
        case _: SecurityException => false
        case _: ClassNotFoundException => false
        case _: NoClassDefFoundError => false
      }
    }
  }

  def testResults = {
    // Test results may either be in a top level project's directory, or else in
    // module directoriy(s)
    (testOutputFile +: upstreamModules.map(_.testOutputFile)).distinct.map(MakerTestResults(_)).reduce(_++_)
  }


  /**
    * To run tests from Vim it is convenient to have _all_ test classes on the classpath,
    * Not just those modules on whom we have a test dependency
    */
  def writeVimClasspath {
    val cp = testCompilationClasspath
    val cpFile : File = file(name + "-classpath.sh")
    println(s"Writing classpath to file $cpFile")
    writeToFile(cpFile, "export CLASSPATH=" + cp + "\n")
  }


  def testClassNames(rootProject : ProjectTrait) : Seq[String]
  def constructorCodeAsString : String = throw new Exception("Only supported by test projects")

  def managedLibDir = file(rootAbsoluteFile, "lib_managed")
  def testManagedLibDir = file(rootAbsoluteFile, "test_lib_managed")
  def managedLibSourceDir = file(rootAbsoluteFile, "lib_src_managed")
  def testManagedLibSourceDir = file(rootAbsoluteFile, "test_lib_src_managed")
  def managedResourceDir = file(rootAbsoluteFile, "resource_managed")
  def unmanagedLibDirs : Seq[File] = List(file(rootAbsoluteFile, "lib"))

  def upstreamDependencies = (upstreamModules ++ testModuleDependencies).distinct.flatMap(_.dependencies)

  protected def managedJars = findJars(managedLibDir)

  def testClasspathLoader = new URLClassLoader(
    testRuntimeClasspathComponents.map(_.toURI.toURL).toArray,
    null
  )
}
