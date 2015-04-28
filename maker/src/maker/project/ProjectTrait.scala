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


  def defaultScalaVersion : ScalaVersion = ScalaVersion.TWO_ELEVEN_DEFAULT

  val rootAbsoluteFile = root.asAbsoluteFile
  lazy val testOutputFile = file(rootAbsoluteFile, "maker-test-output")
  protected def name : String
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

  private def findSingleScalaJar(scalaVersion : ScalaVersion, partName : String) : File = {
    dependencyJars(scalaVersion).filter(_.getName.contains(partName)) match {
      case List(jarFile) => jarFile
      case other => throw new IllegalStateException(s"Expected to find a single scala reflect jar, got $other")
    }
  }
  def scalaReflectJar(scalaVersion : ScalaVersion) = findSingleScalaJar(scalaVersion, "scala-reflect")
  def scalaCompilerJar(scalaVersion : ScalaVersion) = findSingleScalaJar(scalaVersion, "scala-compiler")
  def scalaLibraryJar(scalaVersion : ScalaVersion) = findSingleScalaJar(scalaVersion, "scala-library")
  def scalaXmlJar(scalaVersion : ScalaVersion) : Option[File] = if (scalaVersion.major >= 11) Some(findSingleScalaJar(scalaVersion, "scala-xml")) else None
  def scalaParserCombinatorJar(scalaVersion : ScalaVersion) : Option[File] = if (scalaVersion.major >= 11) Some(findSingleScalaJar(scalaVersion, "scala-parser-combinators")) else None
  def scalaJars(scalaVersion : ScalaVersion) = 
    Vector(scalaReflectJar(scalaVersion), scalaCompilerJar(scalaVersion), scalaLibraryJar(scalaVersion)) ++: 
      scalaXmlJar(scalaVersion).toList ++:
      scalaParserCombinatorJar(scalaVersion).toList 

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
  def tearDown(graph : Dependency.Graph, result : BuildResult) : Boolean = true

  protected def transitiveBuild(rootTasks : Seq[Task]) = {
    Build(
      rootTasks.headOption.map(_.name).getOrElse("Empty build"),
      Dependency.Graph.transitiveClosure(rootTasks, extraUpstreamTasksMatcher, extraDownstreamTasksMatcher),
      config.taskThreadPoolSize
    )
  }


  def compileTaskBuild(scalaVersion : ScalaVersion) = transitiveBuild(modules.map(SourceCompileTask(this, _, scalaVersion)))
  def compile(scalaVersion : ScalaVersion) : BuildResult = execute(compileTaskBuild(scalaVersion))
  def compile : BuildResult = compile(defaultScalaVersion)


  def testCompileTaskBuild(scalaVersion : ScalaVersion) : Build // = transitiveBuild(upstreamModules.map(SourceCompileTask(this, _)) ++ upstreamTestModules.map(TestCompileTask(this, _)))
  def testCompile(scalaVersion : ScalaVersion) = execute(testCompileTaskBuild(scalaVersion))
  def testCompile : BuildResult = testCompile(defaultScalaVersion)
  def tcc = continuously(testCompileTaskBuild(defaultScalaVersion))
  
  def testFailedSuitesBuild(scalaVersion : ScalaVersion) = {
    transitiveBuild(upstreamModules.map(RunUnitTestsTask.failingTests(this, _, scalaVersion)))
  }
  def testFailedSuites(scalaVersion : ScalaVersion) : BuildResult = execute(testFailedSuitesBuild(scalaVersion))
  def testFailedSuites : BuildResult = testFailedSuites(defaultScalaVersion)

  def updateTaskBuild(forceSourceUpdate : Boolean, scalaVersion : ScalaVersion) = {
    transitiveBuild(UpdateTask(this, forceSourceUpdate = forceSourceUpdate, scalaVersion = scalaVersion) :: Nil)
  }
  def update(scalaVersion : ScalaVersion) = execute(updateTaskBuild(forceSourceUpdate = false, scalaVersion = scalaVersion))
  def update : BuildResult = update(defaultScalaVersion)
  def updateSources = execute(updateTaskBuild(forceSourceUpdate = true, scalaVersion = defaultScalaVersion))


  def runMainTaskBuild(className : String, opts : Seq[String], args : Seq[String], scalaVersion : ScalaVersion) = {
    transitiveBuild(RunMainTask(this, className, opts, args, scalaVersion) :: Nil)
  }
  def runMain(className : String, scalaVersion : ScalaVersion)(opts : String*)(args : String*) = 
    execute(runMainTaskBuild(className, opts, args, scalaVersion))
  def runMain(className : String)(opts : String*)(args : String*) : BuildResult = runMain(className, defaultScalaVersion)(opts : _*)(args : _*)

  def clean(scalaVersion : ScalaVersion) = execute(transitiveBuild(CleanTask(this, scalaVersion) :: Nil))
  def clean : BuildResult = clean(defaultScalaVersion)

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

  private [project] def compilationTargetDirectories(scalaVersion : ScalaVersion) = 
    upstreamModules.map(_.classDirectory(scalaVersion))
  private [project] def resourceDirectories = upstreamModules.map(_.resourceDir(SourceCompilePhase))
  private [project] def testResourceDirectories = upstreamModules.map(_.resourceDir(TestCompilePhase))
  private [project] def dependencyJars(scalaVersion : ScalaVersion) = findJars(managedLibDir(scalaVersion))
  private [project] def testDependencyJars(scalaVersion : ScalaVersion) = findJars(testManagedLibDir(scalaVersion))
  private [project] def unmanagedLibs = findJars(upstreamModules.flatMap(_.unmanagedLibDirs))

  def compilationClasspathComponents(scalaVersion : ScalaVersion) = 
      compilationTargetDirectories(scalaVersion) ++:
      dependencyJars(scalaVersion) ++:
      unmanagedLibs

  def compilationClasspath(scalaVersion : ScalaVersion) = Module.asClasspathStr(compilationClasspathComponents(scalaVersion))

  def testCompilationClasspathComponents(scalaVersion : ScalaVersion) = 
      compilationTargetDirectories(scalaVersion) ++:
      testDependencyJars(scalaVersion) ++:
      unmanagedLibs ++:
      (this match {
        case _ : Project => 
          upstreamModules.map(_.testClassDirectory(scalaVersion))
        case m : Module => 
          (m +: testModuleDependencies).map(_.testClassDirectory(scalaVersion))
      })

  def testCompilationClasspath(scalaVersion : ScalaVersion) = 
    Module.asClasspathStr(testCompilationClasspathComponents(scalaVersion))


  def runtimeClasspathComponents(scalaVersion : ScalaVersion) = 
    compilationTargetDirectories(scalaVersion) ++:
    dependencyJars(scalaVersion) ++:
    unmanagedLibs ++:
    resourceDirectories 

  def runtimeClasspath(scalaVersion : ScalaVersion) = Module.asClasspathStr(runtimeClasspathComponents(scalaVersion))


  def testRuntimeClasspathComponents(scalaVersion : ScalaVersion) : Seq[File] =
    compilationTargetDirectories(scalaVersion) ++:
    testDependencyJars(scalaVersion) ++:
    unmanagedLibs ++:
    resourceDirectories ++:
    (this match {
      case _ : Project => 
        upstreamModules.flatMap{m => m.testClassDirectory(scalaVersion) :: m.resourceDir(TestCompilePhase) :: Nil}
      case m : Module => 
        (m +: testModuleDependencies).flatMap{m => m.testClassDirectory(scalaVersion) :: m.resourceDir(TestCompilePhase) :: Nil}
    })

  def testRuntimeClasspath(scalaVersion : ScalaVersion) = 
    Module.asClasspathStr(testRuntimeClasspathComponents(scalaVersion))

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

  def isAccessibleScalaTestSuite(rootProject : ProjectTrait, scalaVersion : ScalaVersion) : (String => Boolean) = {
    val loader = rootProject.testClasspathLoader(scalaVersion)

    className : String => {

      def loadClass(className : String) = {
        try {
          loader.loadClass(className)
        } catch {
          case e : ClassNotFoundException => 
            println(
              s"""Couldn't load class $className in project $this, 
                  classpath was ${testRuntimeClasspathComponents(scalaVersion).mkString("\n\t", "\n\t", "\n")}""")
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
  def writeVimClasspath(scalaVersion : ScalaVersion) {
    val cp = testCompilationClasspath(scalaVersion)
    val cpFile : File = file(name + "-classpath.sh")
    println(s"Writing classpath to file $cpFile")
    writeToFile(cpFile, "export CLASSPATH=" + cp + "\n")
  }


  def testClassNames(rootProject : ProjectTrait, scalaVersion : ScalaVersion) : Seq[String]
  def constructorCodeAsString : String = throw new Exception("Only supported by test projects")

  def managedLibDir(scalaVersion : ScalaVersion) = file(rootAbsoluteFile, "lib_managed", scalaVersion.versionNo)
  def testManagedLibDir(scalaVersion : ScalaVersion) = file(rootAbsoluteFile, "test_lib_managed", scalaVersion.versionNo)
  def managedLibSourceDir(scalaVersion : ScalaVersion) = file(rootAbsoluteFile, "lib_src_managed", scalaVersion.versionNo)
  def testManagedLibSourceDir(scalaVersion : ScalaVersion) = file(rootAbsoluteFile, "test_lib_src_managed", scalaVersion.versionNo)
  def managedResourceDir = file(rootAbsoluteFile, "resource_managed")
  def unmanagedLibDirs : Seq[File] = List(file(rootAbsoluteFile, "lib"))

  def upstreamDependencies = (upstreamModules ++ testModuleDependencies).distinct.flatMap(_.dependencies)

  def testClasspathLoader(scalaVersion : ScalaVersion) = new URLClassLoader(
    testRuntimeClasspathComponents(scalaVersion).map(_.toURI.toURL).toArray,
    null
  )
}
