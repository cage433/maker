package maker.project

import maker.task._
import maker._
import maker.task.compile._
import java.io.File
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
  def config : Config
  def extraUpstreamTasksMatcher : PartialFunction[Task, Set[Task]] = Map.empty
  def extraDownstreamTasksMatcher : PartialFunction[Task, Set[Task]] = Map.empty

  def isTestProject : Boolean 
  def topLevelCompilationErrorsFile = file("vim-compilation-errors")

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
  def upstreamTestModules : Seq[Module] = transitiveClosure(modules, {m : Module => m.immediateUpstreamTestModules})


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


  def testCompileTaskBuild = transitiveBuild(upstreamModules.map(SourceCompileTask(this, _)) ++ upstreamTestModules.map(TestCompileTask(this, _)))
  def testCompile = execute(testCompileTaskBuild)
  def tcc = continuously(testCompileTaskBuild)
  

  def testTaskBuild(verbose : Boolean) = {
    // test is an unusual build, in that if project B depends on project A, then 
    // strictly speaking, to test B it is only necessary to compile A (perhaps test compile also)
    // however - when we run tests on B, in general we also want to run A's tests too. 
    //
    // Use 'testOnly' to just run a single module's tests.
    transitiveBuild(upstreamModules.map(RunUnitTestsTask(_, verbose)))
  }
  def test(verbose : Boolean) : BuildResult = {
    execute(testTaskBuild(verbose))
  }
  def test : BuildResult = {
    test(verbose = false)
  }


  def testFailedSuitesBuild(verbose : Boolean) = {
    transitiveBuild(upstreamModules.map(RunUnitTestsTask.failingTests(_, verbose)))
  }
  def testFailedSuites : BuildResult = execute(testFailedSuitesBuild(verbose = false))

  def updateTaskBuild(forceSourceUpdate : Boolean) = {
    transitiveBuild(upstreamModules.map(UpdateTask(_, forceSourceUpdate = forceSourceUpdate)))
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
    if (result.failed && config.execMode){
      logger.error(bld + " failed ")
      System.exit(-1)
    }
    BuildResult.lastResult.set(Some(result))
    if (! isTestProject){
      ScreenUtils.clear
      result.reportResult
    }
    result
  }

  private [project] def classpathComponents(compilePhase : CompilePhase) = {
    val classFileDirectories : Seq[File] = upstreamModules.map{
      module => 
        module.outputDir(SourceCompilePhase)
    }

    val testClassFileDirectories = compilePhase match {
      case SourceCompilePhase => Nil
      case TestCompilePhase   => 
        upstreamTestModules.map(_.outputDir(TestCompilePhase))
    }
    val jars : Seq[File] = compilePhase match {
      case SourceCompilePhase => 
        findJars(managedLibDir)
      case TestCompilePhase   =>
        findJars(testManagedLibDir)
    }
    managedResourceDir +: jars ++: classFileDirectories ++: testClassFileDirectories
  }

  def classpath(compilePhase : CompilePhase) = {
    Module.asClasspathStr(classpathComponents(compilePhase))
  }

  def continuously(bld : Build){
    var lastTaskTime :Option[Long] = None

    def allSourceFiles : Seq[File] = (upstreamModules ++ upstreamTestModules).distinct.flatMap{
      proj => 
        proj.compilePhase.sourceFiles++ proj.testCompilePhase.sourceFiles
    }

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

  lazy val isAccessibleScalaTestSuite : (String => Boolean) = {
      //(upstreamModules ++: upstreamTestModules).distinct.flatMap{p => p.classpathJars.toSet + p.outputDir(SourceCompilePhase) + p.outputDir(TestCompilePhase)}.map(_.toURI.toURL).toArray,
    lazy val loader = new URLClassLoader(
      classpathComponents(TestCompilePhase).map(_.toURI.toURL).toArray,
      null
    )
    (className: String) =>  {
      val suiteClass = loader.loadClass("org.scalatest.Suite")
      val emptyClassArray = new Array[java.lang.Class[T] forSome {type T}](0)
      val clazz = loader.loadClass(className)
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
    //var dirsAndJars = upstreamModules.flatMap(_.testCompilePhase.classpathDirectoriesAndJars).toList.distinct
    //dirsAndJars ::= config.scalaVersion.scalaCompilerJar
    //dirsAndJars ::= config.scalaVersion.scalaLibraryJar
    val cp = classpath(TestCompilePhase)
    val cpFile : File = file(name + "-classpath.sh")
    writeToFile(cpFile, "export CLASSPATH=" + cp + "\n")
  }


  def testClassNames() : Seq[String]
  def constructorCodeAsString : String = throw new Exception("Only supported by test projects")

  def managedLibDir = file(rootAbsoluteFile, "lib_managed")
  def testManagedLibDir = file(rootAbsoluteFile, "test_lib_managed")
  def managedLibSourceDir = file(rootAbsoluteFile, "lib_src_managed")
  def testManagedLibSourceDir = file(rootAbsoluteFile, "test_lib_src_managed")
  def managedResourceDir = file(rootAbsoluteFile, "resource_managed")
  def unmanagedLibDirs : Seq[File] = List(file(rootAbsoluteFile, "lib"))

  def upstreamResources = upstreamModules.flatMap(_.resources)

  protected def managedJars = findJars(managedLibDir)
  //def classpathJars : Seq[File] = findJars(managedLibDir +: unmanagedLibDirs) ++:
    //Vector[File](config.scalaVersion.scalaLibraryJar, config.scalaVersion.scalaCompilerJar) ++: config.scalaVersion.scalaReflectJar.toVector
}
