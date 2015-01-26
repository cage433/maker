package maker.project

import maker.task._
import maker.{MakerProps, Resource}
import maker.task.compile._
import java.io.File
import maker.utils.{FileUtils, MakerTestResults, ScreenUtils}
import maker.utils.FileUtils._
import maker.task.tasks._
import maker.utils.RichString._
import java.net.URLClassLoader
import java.lang.reflect.Modifier
import maker.ivy.IvyUtils
import scala.xml.Elem
import org.slf4j.LoggerFactory

trait BaseProject {
  protected def root : File
  val rootAbsoluteFile = root.asAbsoluteFile
  lazy val testOutputFile = file(rootAbsoluteFile, "maker-test-output")
  def name : String
  def setUp(graph : Dependency.Graph) : Boolean = {
    if (graph.includesCompileTask){
      props.VimErrorFile().delete
    }
    ! props.VimErrorFile().exists()
  }
  def tearDown(graph : Dependency.Graph, result : BuildResult) : Boolean
  def extraUpstreamTasks(task : Task) : Set[Task] =
    extraUpstreamTasksMatcher.lift(task).getOrElse(Set.empty)
  def extraUpstreamTasksMatcher : PartialFunction[Task, Set[Task]] = Map.empty
  def extraDownstreamTasks(task : Task) : Set[Task] =
    extraDownstreamTasksMatcher.lift(task).getOrElse(Set.empty)
  def extraDownstreamTasksMatcher : PartialFunction[Task, Set[Task]] = Map.empty
  def props : MakerProps

  lazy val allStrictlyUpstreamModules : List[Module] = immediateUpstreamModules.flatMap(_.allUpstreamModules).distinct.sortWith(_.name < _.name)

  def immediateUpstreamModules : List[Module]
  def immediateUpstreamTestModules : List[Module]
  def allUpstreamModules : List[Module]
  def allUpstreamTestModules : List[Module]
  def allUpstreamBaseProjects : List[BaseProject] = (this :: allStrictlyUpstreamModules).sortWith(_.name < _.name)

  lazy val groupId = props.GroupId()
  val artifactId = name
  def toIvyExclude : Elem = <exclude org={groupId} module={artifactId} />
  def publishLocalDir = file(props.PublishLocalRootDir(), groupId, artifactId).makeDirs
  def publishLocalPomFile = file(file(publishLocalDir, "/poms/").makeDir, "pom.xml")
  def packageDir : File
  def packageJar(compilePhase : CompilePhase) = {
    val jarBasename = compilePhase match {
      case SourceCompilePhase => name + ".jar"
      case TestCompilePhase => name + "-test.jar"
    }
    file(packageDir.getAbsolutePath, jarBasename)
  }

  def publishLocal(version : String, signArtifacts : Boolean = false, includeUpstreamModules : Boolean = false) = {
    val tasks = if (includeUpstreamModules)
        PublishLocalTask(this, allUpstreamModules, version, signArtifacts) :: Nil
      else
        PublishLocalTask(this, Nil, version, signArtifacts) :: allUpstreamModules.map{m => PublishLocalTask(m, Vector(m), version, signArtifacts)}

    execute(Build.apply(props.NumberOfTaskThreads(), tasks : _*))
  }
  def publish(version : String, resolver : String, signArtifacts : Boolean = false, includeUpstreamModules : Boolean = false) = {
    val tasks = if (includeUpstreamModules)
        PublishTask(this, allUpstreamModules, resolver, version, signArtifacts) :: Nil
      else
        PublishTask(this, Nil, resolver, version, signArtifacts) :: allUpstreamModules.map{m => PublishTask(m, Vector(m), resolver, version, signArtifacts)}

    execute(Build.apply(props.NumberOfTaskThreads(), tasks : _*))
  }
  def sourcePackageJar(compilePhase : CompilePhase) = {
    val jarBasename = compilePhase match {
      case SourceCompilePhase => name + "-sources.jar"
      case TestCompilePhase => name + "-test-sources.jar"
    }
    file(packageDir.getAbsolutePath, jarBasename)
  }
  def docPackageJar = file(packageDir.getAbsolutePath, name + "-javadoc.jar")

  def ivySettingsFile = file("ivysettings.xml") // Note that this is relative to CWD
  def ivyFile = IvyUtils.generateIvyFile(this)
  def projectTypeName = this.getClass.getSimpleName // 'Module' or 'Project# 

  def testResults = {
    // Test results may either be in a top level project's directory, or else in
    // module directoriy(s)
    (testOutputFile::allUpstreamModules.map(_.testOutputFile)).toList.distinct.map(MakerTestResults(props, _)).reduce(_++_)
  }

  def testClasspath = Module.asClasspathStr(
    allUpstreamModules.flatMap(_.testCompilePhase.classpathDirectoriesAndJars)
  )

  def testClassNames() : Iterable[String]

  def docOutputDir : File

  /**
    * Makes a Build that is the closure of the task applied to 
    * upstream modules
    */
  def moduleBuild(task : Module => Task, upstreamModules : List[Module] = upstreamModulesForBuild) = 
    Build.apply(props.NumberOfTaskThreads(), upstreamModules.map(task) : _*)

  /** modules that need to be taken into consideration
    * when doing a transitive build */
  protected def upstreamModulesForBuild : List[Module]

  protected def executeWithDependencies(
    task : Module => Task, 
    upstreamModules : List[Module] = upstreamModulesForBuild
  ) : BuildResult = 
  {
    execute(moduleBuild(task, upstreamModules))
  }

  /**
    * Makes a Build that is the closure of some task. This project is
    * passed in in case it has any extra tasks
    */
  protected def taskBuild(task : Task) = {
    Build(
      task.name,
      Dependency.Graph.transitiveClosure(task),
      props.NumberOfTaskThreads()
    )
  }

  protected def executeWithDependencies(task : Task) = { 
    execute(taskBuild(task))
  }

  def clean = executeWithDependencies(CleanTask(_))
  def cleanAll = executeWithDependencies(CleanTask(_, deleteManagedLibs = true))

  def compile = executeWithDependencies(SourceCompileTask(_))
  def compileContinuously = continuously(moduleBuild(SourceCompileTask(_)))

  def testCompile = executeWithDependencies(TestCompileTask(_))
  def testCompileContinuously = continuously(moduleBuild(TestCompileTask(_)))

  private[project] def testBuild(verbose : Boolean) = {
    // test is an unusual build, in that if project B depends on project A, then 
    // strictly speaking, to test B it is only necessary to compile A (perhaps test compile also)
    // however - when we run tests on B, in general we also want to run A's tests too. 
    //
    // Use 'testOnly' to just run a single module's tests.
    moduleBuild(RunUnitTestsTask(_, verbose), allUpstreamModules)
  }
  def test(verbose : Boolean) : BuildResult = {
    execute(testBuild(verbose))
  }
  def test : BuildResult = test(verbose = false)

  def testClass(className : String, verbose : Boolean = false) = executeWithDependencies(
    RunUnitTestsTask(this, verbose, className)
  )
  def testClassContinuously(className : String) = {
    continuously(taskBuild(RunUnitTestsTask(this, false, className)))
  }

  def testFailedSuites(verbose : Boolean) : BuildResult = {
    // To be consistent with 'test' - build must be against all upstream modules
    val build = moduleBuild(
      RunUnitTestsTask.failingTests(_, verbose), allUpstreamModules
    )
    execute(build)
  }
  def testFailedSuites : BuildResult = testFailedSuites(verbose = false)

  def pack(includeUpstreamModules : Boolean = false) : BuildResult = {
    val tasks = if (includeUpstreamModules)
        PackageJarTask(this, allUpstreamModules, SourceCompilePhase) :: Nil
      else
        allUpstreamModules.map{m => PackageJarTask(m, Vector(m), SourceCompilePhase)}

    execute(Build.apply(props.NumberOfTaskThreads(), tasks : _*))
  }

  def update = execute(moduleBuild(UpdateTask(_, forceSourceUpdate = false), allUpstreamModules))
  def updateSources = execute(moduleBuild(UpdateTask(_, forceSourceUpdate = true), allUpstreamModules))

  def missingSourceJars() : List[Resource] = {
    for {
      module <- allUpstreamModules
      srcJar <- module.sourceJarResources()
      if !srcJar.resourceFile.exists
    } yield srcJar
  }

  def createDeploy(buildTests: Boolean = true, version: Option[String] = None): BuildResult =
    throw new UnsupportedOperationException


  def runMain(className : String)(opts : String*)(args : String*) = 
    executeWithDependencies(RunMainTask(this, className, opts.toList, args.toList))


  // Some sugar
  def tcc = testCompileContinuously
  def stfe {
    props.ShowFailingTestException := ! props.ShowFailingTestException()
  }

  protected def execute(bld : Build) = {
    setUp(bld.graph)
    val result = bld.execute
    tearDown(bld.graph, result)
    if (result.failed && props.ExecMode()){
      BaseProject.logger.error(bld + " failed ")
      System.exit(-1)
    }
    BuildResult.lastResult.set(Some(result))
    if (! props.RunningInMakerTest()){
      ScreenUtils.clear
      result.reportResult
    }
    result
  }

  def continuously(bld : Build){
    var lastTaskTime :Option[Long] = None

    def allSourceFiles : List[File] = allUpstreamModules.flatMap{
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


    def lastSrcModificationTime = {
      allUpstreamModules.map(proj => {
          val watchedFiles = proj.compilePhase.sourceFiles ++ proj.testCompilePhase.sourceFiles
          FileUtils.lastModifiedFileTime(watchedFiles)
        }).max
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
    lazy val loader = new URLClassLoader(
      allUpstreamModules.flatMap{p => p.classpathJars.toSet + p.outputDir(SourceCompilePhase) + p.outputDir(TestCompilePhase)}.map(_.toURI.toURL).toArray,
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

  /**
    * To run tests from Vim it is convenient to have _all_ test classes on the classpath,
    * Not just those modules on whom we have a test dependency
    */
  def writeVimClasspath {
    var dirsAndJars = allUpstreamModules.flatMap(_.testCompilePhase.classpathDirectoriesAndJars).toList.distinct
    dirsAndJars ::= props.ProjectScalaCompilerJar()
    dirsAndJars ::= props.ProjectScalaLibraryJar()
    val cp = Module.asClasspathStr(dirsAndJars)
    val cpFile : File = file(name + "-classpath.sh")
    writeToFile(cpFile, "export CLASSPATH=" + cp + "\n")
    appendToFile(cpFile, "export JAVA_OPTS=\" " + props.MakerHome.toCommandLine + " \"\n")
  }

  def constructorCodeAsString : String

  def writeMakerProjectDefinitionFile{
    val makerFile = file(rootAbsoluteFile, "Maker.scala")

    val buffer = new StringBuffer
    buffer.addLine("import maker.project.Module._")
    buffer.addLine("import maker.task.tasks._")
    buffer.addLine("import maker.task._")
    buffer.addLine("import maker.task.Dependency._")
    buffer.addLine("import maker.project._")
    buffer.addLine("import maker.utils.FileUtils._")
    buffer.addLine("import java.io.File")
    buffer.addLine(constructorCodeAsString)
    buffer.addLine("import " + name + "._")
    writeToFile(makerFile, buffer.toString)
  }

  def delete = recursiveDelete(rootAbsoluteFile)
}

object BaseProject{
  val logger = LoggerFactory.getLogger(this.getClass)
}
