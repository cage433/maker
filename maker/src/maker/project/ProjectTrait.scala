package maker.project

import maker.task._
import maker._
import maker.task.compile._
import java.io._
import maker.utils._
import maker.utils.FileUtils._
import maker.task.tasks._
import maker.utils.RichString._
import java.net.{URLClassLoader, ServerSocket}
import java.lang.reflect.Modifier
import scala.xml.{Elem, NodeSeq}
import org.slf4j.LoggerFactory
import scala.collection.immutable.Nil
import org.apache.commons.io.output.TeeOutputStream
import java.util.concurrent.atomic.AtomicReference

trait ProjectTrait extends MakerConfig{
  lazy val logger = LoggerFactory.getLogger(getClass)
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
  protected def name : String
  def modules : Seq[Module]
  def testModuleDependencies : Seq[Module]
  def extraUpstreamTasksMatcher : PartialFunction[Task, Set[Task]] = Map.empty
  def extraDownstreamTasksMatcher : PartialFunction[Task, Set[Task]] = Map.empty


  private def findSingleScalaJar(scalaVersion : ScalaVersion, partName : String) : File = {
    dependencyJars(scalaVersion, SourceCompilePhase).filter(_.getName.contains(partName)) match {
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

  /* Methods that are overriden by maker unit tests projects/modules */
  def reportBuildResult : Boolean = true
  def systemExitOnExecModeFailures : Boolean = true
  def updateIncludesSourceJars : Boolean = true

  private var dependenciesUpdated = Set[ScalaVersion]()
  def markDependenciesUpdated(scalaVersion : ScalaVersion) = {
    dependenciesUpdated += scalaVersion
  }
  def dependenciesAlreadyUpdated(scalaVersion : ScalaVersion) = dependenciesUpdated.contains(scalaVersion)
  def clearDependencies(scalaVersion : ScalaVersion) = {
    dependenciesUpdated -= scalaVersion

    cleanRegularFilesLeavingDirectories(managedLibDir(scalaVersion))
    cleanRegularFilesLeavingDirectories(managedLibSourceDir(scalaVersion))
    cleanRegularFilesLeavingDirectories(testManagedLibDir(scalaVersion))
    cleanRegularFilesLeavingDirectories(testManagedLibSourceDir(scalaVersion))
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
  def upstreamModules : Seq[Module] = transitiveClosure(modules, {m : Module => m.immediateUpstreamModules})

  def setUp(graph : Dependency.Graph) : Boolean = {
    if (graph.includesCompileTask){
      CompileTask.topLevelCompilationErrorsFile.delete
    }
    ! CompileTask.topLevelCompilationErrorsFile.exists()
  }
  protected def tearDown(graph : Dependency.Graph, result : BuildResult) : Boolean = true

  protected def taskThreadPoolSize: Option[Int] = None
  protected def transitiveBuild(rootTasks : Seq[Task]) = {
    Build(
      rootTasks.headOption.map(_.name).getOrElse("Empty build"),
      Dependency.Graph.transitiveClosure(rootTasks, extraUpstreamTasksMatcher, extraDownstreamTasksMatcher),
      taskThreadPoolSize
    )
  }


  def compileTaskBuild(scalaVersion : ScalaVersion) = transitiveBuild(modules.map(CompileTask(this, _, scalaVersion, SourceCompilePhase)))
  def compile(scalaVersion : ScalaVersion) : BuildResult = execute(compileTaskBuild(scalaVersion))
  def compile : BuildResult = compile(defaultScalaVersion)


  def testCompileTaskBuild(scalaVersion : ScalaVersion, testPhases : Seq[CompilePhase]) : Build // = transitiveBuild(upstreamModules.map(SourceCompileTask(this, _)) ++ upstreamTestModules.map(TestCompileTask(this, _)))
  def testCompile : BuildResult = execute(testCompileTaskBuild(defaultScalaVersion, CompilePhase.TEST_PHASES))
  def tcc = continuously(() => testCompileTaskBuild(defaultScalaVersion, CompilePhase.TEST_PHASES))
  
  def testFailedSuitesBuild(scalaVersion : ScalaVersion) = {
    transitiveBuild(upstreamModules.map(RunUnitTestsTask.failingTests(this, _, scalaVersion)))
  }
  def testFailedSuites(scalaVersion : ScalaVersion) : BuildResult = execute(testFailedSuitesBuild(scalaVersion))
  def testFailedSuites : BuildResult = testFailedSuites(defaultScalaVersion)

  def updateTaskBuild(scalaVersion : ScalaVersion) = {
    transitiveBuild(UpdateTask(this, scalaVersion) :: Nil)
  }
  def update(scalaVersion : ScalaVersion) = execute(updateTaskBuild(scalaVersion))
  def update : BuildResult = update(defaultScalaVersion)

  def forceUpdate(scalaVersion : ScalaVersion) : BuildResult = {
    clearDependencies(scalaVersion)
    update(scalaVersion)
  }
  def forceUpdate : BuildResult = forceUpdate(defaultScalaVersion)


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
    if (result.failed && runningInExecMode && systemExitOnExecModeFailures){
      logger.error(bld + " failed - exiting")
      System.exit(1)
    }
    BuildResult.lastResult.set(Some(result))
    if(reportBuildResult)
      result.reportResult
    result
  }

  private [project] def compilationTargetDirectories(scalaVersion : ScalaVersion, phases : Seq[CompilePhase]) = {
    val phases_ = (SourceCompilePhase +: phases).distinct
    phases_.flatMap{
      phase => 
        phase match {
          case SourceCompilePhase => 
            upstreamModules.map(_.classDirectory(scalaVersion, phase))
          case _ => 
            (this match {
              case _ : Project => 
                upstreamModules.map(_.classDirectory(scalaVersion, phase))
              case m : Module => 
                (m +: upstreamModules.flatMap(_.testModuleDependencies)).map(_.classDirectory(scalaVersion, phase))
            })
        }
    }
  }

  private [project] def resourceDirectories(phases : Seq[CompilePhase]) = upstreamModules.flatMap{
    module => 
      phases.map(module.resourceDir(_))
  }

  private [project] def dependencyJars(scalaVersion : ScalaVersion, phase : CompilePhase) = phase match {
    case SourceCompilePhase => 
      findJars(managedLibDir(scalaVersion))
    case _ => 
      findJars(testManagedLibDir(scalaVersion))
  }
  private [project] def unmanagedLibs = findJars(upstreamModules.flatMap(_.unmanagedLibDirs))

  def compilationClasspathComponents(scalaVersion : ScalaVersion, phase : CompilePhase) = 
      compilationTargetDirectories(scalaVersion, phase :: Nil) ++:
      dependencyJars(scalaVersion, phase) ++:
      unmanagedLibs

  def compilationClasspath(scalaVersion : ScalaVersion, phase : CompilePhase) = 
    Module.asClasspathStr(compilationClasspathComponents(scalaVersion, phase))


  def runtimeClasspathComponents(scalaVersion : ScalaVersion, phases : Seq[CompilePhase]) = 
    compilationTargetDirectories(scalaVersion, phases) ++:
    (if (phases.toSet.intersect(CompilePhase.TEST_PHASES.toSet).nonEmpty)
      dependencyJars(scalaVersion, TestCompilePhase) 
    else
      dependencyJars(scalaVersion, SourceCompilePhase)
    ) ++:
    unmanagedLibs ++:
    resourceDirectories(phases) 

  def runtimeClasspath(scalaVersion : ScalaVersion, phases : Seq[CompilePhase]) = Module.asClasspathStr(runtimeClasspathComponents(scalaVersion, phases))



  def continuously(bld : () => Build) {
    var lastTaskTime :Option[Long] = None

    def allSourceFiles : Seq[File] = upstreamModules.flatMap(_.sourceFiles(SourceCompilePhase)) ++: 
    (upstreamModules.flatMap{module => module +: module.testModuleDependencies}).distinct.flatMap{module => 
      CompilePhase.TEST_PHASES.flatMap(module.sourceFiles(_))
    }

    def sourceFileCount : Int = allSourceFiles.size
    var lastFileCount : Int = sourceFileCount 
    def sourceFileNames : String = allSourceFiles.map(_.getPath).sortWith(_<_).mkString(" ")
    var lastSourceFileNames : String = sourceFileNames

    def printWaitingMessage = println("\nWaiting for source file changes (press 'enter' to interrupt)")
    def rerunTask{
      println(execute(bld()))
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

  def isAccessibleScalaTestSuite(rootProject : ProjectTrait, scalaVersion : ScalaVersion, testPhase : CompilePhase) : (String => Boolean) = {
    val loader = rootProject.testClasspathLoader(scalaVersion, testPhase)

    className : String => {

      def loadClass(className : String) = {
        try {
          loader.loadClass(className)
        } catch {
          case e : ClassNotFoundException => 
            println(
              s"""Couldn't load class $className in project $this, 
                  classpath was ${runtimeClasspathComponents(scalaVersion, testPhase :: Nil).mkString("\n\t", "\n\t", "\n")}""")
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
    upstreamModules.distinct.map(MakerTestResults(_)).reduce(_++_)
  }


  /**
    * To run tests from Vim it is convenient to have _all_ test classes on the classpath,
    * Not just those modules on whom we have a test dependency
    */
  def writeVimClasspath(scalaVersion : ScalaVersion) {
    var components = CompilePhase.PHASES.flatMap{phase => 
      compilationTargetDirectories(scalaVersion, phase :: Nil)
    }.distinct ++: resourceDirectories(CompilePhase.PHASES)
    components ++= dependencyJars(scalaVersion, TestCompilePhase)
    components ++= unmanagedLibs
    components ++= managedLibDir(scalaVersion) :: Nil
    components = components.filter{
      file =>
        file.exists && (!file.isDirectory || file.list.size > 0)
    }
    var relativePaths = components.map(_.relativeTo(file("."))).map(_.getPath)
    relativePaths = relativePaths.map{
      path => 
        if (path.endsWith(".jar")) {
          val pathComponents = path.split(File.separator)
          (pathComponents.dropRight(1) :+ "*").mkString(File.separator)
        } else 
          path
    }.distinct
    val cp = relativePaths.mkString(File.pathSeparator)
    val cpFile : File = file("maker-classpath.txt")
    println(s"Writing classpath to file $cpFile")
    writeToFile(cpFile, cp)
  }

  def writeVimClasspath {
    writeVimClasspath(defaultScalaVersion)
  }

  def testClassNames(rootProject : ProjectTrait, scalaVersion : ScalaVersion, lastCompilationTime : Option[Long], testPhase : CompilePhase) : Seq[String]
  def constructorCodeAsString : String = throw new Exception("Only supported by test projects")

  def managedLibDir(scalaVersion : ScalaVersion) = file(rootAbsoluteFile, "lib_managed", scalaVersion.versionNo)
  def testManagedLibDir(scalaVersion : ScalaVersion) = file(rootAbsoluteFile, "test_lib_managed", scalaVersion.versionNo)
  def managedLibSourceDir(scalaVersion : ScalaVersion) = file(rootAbsoluteFile, "lib_src_managed", scalaVersion.versionNo)
  def testManagedLibSourceDir(scalaVersion : ScalaVersion) = file(rootAbsoluteFile, "test_lib_src_managed", scalaVersion.versionNo)
  def managedResourceDir = file(rootAbsoluteFile, "resource_managed")
  def unmanagedLibDirs : Seq[File] = List(file(rootAbsoluteFile, "lib"))

  def upstreamDependencies = (upstreamModules ++ upstreamModules.flatMap(_.testModuleDependencies)).distinct.flatMap(_.dependencies)

  def testClasspathLoader(scalaVersion : ScalaVersion, testPhase : CompilePhase) = new URLClassLoader(
    runtimeClasspathComponents(scalaVersion, testPhase :: Nil).map(_.toURI.toURL).toArray,
    null
  )
  def testTaskBuild(scalaVersion : ScalaVersion, lastCompilationTimeFilter : Option[Long]) : Build

  def test(scalaVersion : ScalaVersion) : BuildResult = {
    execute(testTaskBuild(scalaVersion, lastCompilationTimeFilter = None))
  }

  def test : BuildResult = test(defaultScalaVersion)

  def testQuick(scalaVersion : ScalaVersion) : BuildResult = {
    execute(testTaskBuild(scalaVersion, lastCompilationTimeFilter = Some(System.currentTimeMillis)))
  }

  def testQuick : BuildResult = testQuick(defaultScalaVersion)

  def testQuickContinuously = continuously(() => testTaskBuild(defaultScalaVersion, lastCompilationTimeFilter = Some(System.currentTimeMillis)))
  def tqc = testQuickContinuously

  def listenForCommands(port : Int){
    val server = new ServerSocket(port)
    var shouldDie = false
    while (!shouldDie){
      val socket = server.accept()
      val in = new BufferedReader(new InputStreamReader(socket.getInputStream()))
      val out = new PrintWriter(socket.getOutputStream(), true)
      in.readLine() match {
        case "DIE" => 
          shouldDie = true
          out.println("DONE")
        case "COMPILE" =>
          val succeeded = try {
            testCompile.succeeded
          } catch {
            case e : Exception => 
              println(e.getMessage)
              false
          }

          if (succeeded)
            out.println("SUCCEEDED")
          else
            out.println("FAILED")
        case other => 
          println(s"Unexpected command $other")
      }
      in.close
      out.close
    }
    server.close
  }

}
