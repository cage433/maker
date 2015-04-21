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

trait BaseProject extends ConfigPimps {
  protected def root : File
  def config : Config
  val rootAbsoluteFile = root.asAbsoluteFile
  lazy val testOutputFile = file(rootAbsoluteFile, "maker-test-output")
  def name : String

  def setUp(graph : Dependency.Graph) : Boolean = {
    if (graph.includesCompileTask){
      topLevelCompilationErrorsFile.delete
    }
    ! topLevelCompilationErrorsFile.exists()
  }
  def tearDown(graph : Dependency.Graph, result : BuildResult) : Boolean

  def extraUpstreamTasksMatcher : PartialFunction[Task, Set[Task]] = Map.empty
  def extraDownstreamTasksMatcher : PartialFunction[Task, Set[Task]] = Map.empty

  //lazy val allStrictlyUpstreamModules : List[Module] = immediateUpstreamModules.flatMap(_.allUpstreamModules).distinct.sortWith(_.name < _.name)

  def immediateUpstreamModules : List[Module]
  def immediateUpstreamTestModules : List[Module]
  //def allUpstreamModules : List[Module]
  //def allUpstreamTestModules : List[Module]
  //def allUpstreamBaseProjects : List[BaseProject] = (this :: allStrictlyUpstreamModules).sortWith(_.name < _.name)

  // This needs to be overriden if this module is to be published to maven/nexus
  def organization : Option[String] = None
  val artifactId = name

  def toIvyExclude : Elem = <exclude org={organization.getOrElse(???)} module={artifactId} />
  def publishLocalDir(version : String) = file(publishLocalRootDir, organization.getOrElse(???), artifactId, version).makeDirs
  def publishLocalJarDir(version : String) = file(publishLocalDir(version), "jars").makeDir
  def publishLocalPomDir(version : String) = file(publishLocalDir(version), "poms").makeDir
  def publishLocalPomFile(version : String) = file(publishLocalPomDir(version), s"pom.xml")
  def packageDir : File
  def packageJar(compilePhase : CompilePhase, version : Option[String]) = {
    val versionAsString = version.map("-" + _).getOrElse("")
    val jarBasename = compilePhase match {
      case SourceCompilePhase => name + versionAsString + ".jar"
      case TestCompilePhase => name + versionAsString + "-test.jar"
    }
    file(packageDir.getAbsolutePath, jarBasename)
  }


  //def publishLocal(version : String, signArtifacts : Boolean = false, includeUpstreamModules : Boolean = false) = {
    //val tasks = if (includeUpstreamModules)
        //PublishLocalTask(this, allUpstreamModules, version, signArtifacts) :: Nil
      //else
        //PublishLocalTask(this, Nil, version, signArtifacts) :: allUpstreamModules.map{m => PublishLocalTask(m, Vector(m), version, signArtifacts)}

    //executeWithDependencies(tasks: _*)
  //}


  def sourcePackageJar(compilePhase : CompilePhase, version : Option[String]) = {
    val versionAsString = version.map("-" + _).getOrElse("")
    val jarBasename = compilePhase match {
      case SourceCompilePhase => name + versionAsString + "-sources.jar"
      case TestCompilePhase => name + versionAsString + "-test-sources.jar"
    }
    file(packageDir.getAbsolutePath, jarBasename)
  }
  def docPackageJar = file(packageDir.getAbsolutePath, name + "-javadoc.jar")

  def projectTypeName = this.getClass.getSimpleName // 'Module' or 'Project# 

  //def testResults = {
    //// Test results may either be in a top level project's directory, or else in
    //// module directoriy(s)
    //(testOutputFile::allUpstreamModules.map(_.testOutputFile)).toList.distinct.map(MakerTestResults(_)).reduce(_++_)
  //}

  //def testClasspath = Module.asClasspathStr(
    //allUpstreamModules.flatMap(_.testCompilePhase.classpathDirectoriesAndJars)
  //)

  //def testClassNames() : Iterable[String]

  def docOutputDir : File

  /**
    * Makes a Build that is the closure of the task applied to 
    * upstream modules
    */
  private[project] def moduleBuild(task : Module => Task) = 
    transitiveBuild(upstreamModulesForBuild.map(task) : _*)

  /** modules that need to be taken into consideration
    * when doing a transitive build */
  protected def upstreamModulesForBuild : List[Module]
  protected def defaultRootBuildModules : List[Module] = upstreamModulesForBuild

  protected def executeWithDependencies(
    task : Module => Task
  ) : BuildResult = 
  {
    execute(moduleBuild(task))
  }

  /**
    * Makes a Build that is the closure of some task. 
    */
  private def transitiveBuild(tasks : Task*) = {
    Build(
      tasks.headOption.map(_.name).getOrElse("Empty build"),
      Dependency.Graph.transitiveClosure(tasks.toList, extraUpstreamTasksMatcher, extraDownstreamTasksMatcher),
      config.taskThreadPoolSize
    )
  }

  protected def executeWithDependencies(tasks : Task*) = { 
    execute(transitiveBuild(tasks : _*))
  }

  def clean = executeWithDependencies(CleanTask(_))




  //def runTestContinuously(className : String, verbose : Boolean = false) = {
    //val build = transitiveBuild(RunUnitTestsTask(this, verbose, className))
    //continuously(build)
  //}
  //def rtc(className : String, verbose : Boolean = false) = runTestContinuously(className, verbose)

  private[project] def testBuild(verbose : Boolean) = {
    // test is an unusual build, in that if project B depends on project A, then 
    // strictly speaking, to test B it is only necessary to compile A (perhaps test compile also)
    // however - when we run tests on B, in general we also want to run A's tests too. 
    //
    // Use 'testOnly' to just run a single module's tests.
    moduleBuild(RunUnitTestsTask(_, verbose))
  }
  def test(verbose : Boolean) : BuildResult = {
    execute(testBuild(verbose))
  }
  def test : BuildResult = test(verbose = false)

  //def testClass(className : String, verbose : Boolean = false) = executeWithDependencies(
    //RunUnitTestsTask(this, verbose, className)
  //)
  //def testClassContinuously(className : String) = {
    //continuously(transitiveBuild(RunUnitTestsTask(this, false, className)))
  //}

  def testFailedSuites(verbose : Boolean) : BuildResult = {
    // To be consistent with 'test' - build must be against all upstream modules
    val build = moduleBuild(RunUnitTestsTask.failingTests(_, verbose))
    execute(build)
  }
  def testFailedSuites : BuildResult = testFailedSuites(verbose = false)

  //def pack(includeUpstreamModules : Boolean = false) : BuildResult = {
    //val tasks = if (includeUpstreamModules)
        //PackageJarTask(this, upstreamModules, SourceCompilePhase, version = None) :: Nil
      //else
        //allUpstreamModules.map{m => PackageJarTask(m, Vector(m), SourceCompilePhase, version = None)}

    //execute(transitiveBuild(tasks : _*))
  //}

  private def updateBuild(forceSourceUpdate : Boolean) = {
    transitiveBuild(defaultRootBuildModules.map(UpdateTask(_, forceSourceUpdate = forceSourceUpdate)) : _*)
  }
  def update = execute(updateBuild(forceSourceUpdate = false))
  def updateSources = execute(updateBuild(forceSourceUpdate = true))


  //def runMain(className : String)(opts : String*)(args : String*) = 
    //executeWithDependencies(RunMainTask(this, className, opts.toList, args.toList))



  protected def execute(bld : Build) = {
    setUp(bld.graph)
    val result = bld.execute
    tearDown(bld.graph, result)
    if (result.failed && config.execMode){
      BaseProject.logger.error(bld + " failed ")
      System.exit(-1)
    }
    BuildResult.lastResult.set(Some(result))
    if (! isTestProject){
      ScreenUtils.clear
      result.reportResult
    }
    result
  }



  def delete = recursiveDelete(rootAbsoluteFile)

  def extraProjectPomInfo : List[NodeSeq] = Nil

  // Overriden by maker unit tests to quieten output
  def isTestProject : Boolean = false

  def topLevelCompilationErrorsFile = file("vim-compilation-errors")

  def publishLocalRootDir  = file(System.getenv("HOME"), ".maker", "publish-local")

}

object BaseProject{
  lazy val logger = LoggerFactory.getLogger(this.getClass)
}
