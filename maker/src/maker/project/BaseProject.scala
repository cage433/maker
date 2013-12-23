package maker.project
import maker.build.Dependency
import maker.build.Dependency.Graph
import maker.build.BuildResult
import maker.task.Task
import maker.Props
import maker.build.Build
import maker.task.compile.SourceCompileTask
import maker.task.tasks.CleanTask
import java.io.File
import maker.utils.FileUtils
import maker.utils.FileUtils._
import maker.task.compile.TestCompileTask
import maker.task.tasks._
import maker.task.test.RunUnitTestsTask
import maker.utils.Implicits.RichString._
import java.net.URLClassLoader
import java.lang.reflect.Modifier
import maker.task.update.Resource
import scala.xml.Elem
import maker.Help
import maker.task.test.TestResults
import maker.task.publish.IvyUtils
import maker.task.publish.PublishLocalTask
import maker.task.publish.PublishTask
import maker.task.test.AkkaTestManager
import java.util.concurrent.atomic.AtomicReference
import maker.build.BuildManager.TimedResults
import maker.utils.os.Command
import maker.utils.os.CommandOutputHandler
import scala.actors.Future
import scala.actors.Futures

trait BaseProject {
  protected def root : File
  val rootAbsoluteFile = root.asAbsoluteFile
  def name : String
  def setUp(graph : Dependency.Graph) : Unit
  def tearDown(graph : Dependency.Graph, result : TimedResults) : Unit
  def extraUpstreamTasks(task : Task) : Set[Task] = Set.empty
  def extraDownstreamTasks(task : Task) : Set[Task] = Set.empty
  def props : Props
  def log = props.log

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

  def ivyFile = IvyUtils.generateIvyFile(this)
  def projectTypeName = this.getClass.getSimpleName // 'Module' or 'Project# 

  def buildTestManager() = new AkkaTestManager(this)

  def testClasspath = Module.asClasspathStr(
    allUpstreamModules.flatMap(_.testCompilePhase.classpathDirectoriesAndJars)
  )

  def testClassNames() : Iterable[String]

  def docOutputDir : File

  val lastTestResults : AtomicReference[Option[TestResults]] = new AtomicReference(None)

  private def buildName(text : String) = {
    text + " " + getClass.getSimpleName.toLowerCase + " " + name
  }

  lazy val Clean = Build(
    buildName("Clean"),
    () => Dependency.Graph.transitiveClosure(this, allUpstreamModules.map(CleanTask(_))),
    this,
    "clean",
    """
    |Cleans this and upstream modules. Including class files, caches, output jars and doc but excluding managed libraries
    |
    |see also CleanAll and CleanOnly
    """.stripMargin
  )

  lazy val CleanAll = Build(
    "Clean All " + name, 
    () => Dependency.Graph.transitiveClosure(this, allUpstreamModules.map(CleanTask(_, deleteManagedLibs = true))),
    this,
    "cleanAll",
    "Same as clean except it also deletes managed libraries"
  )

  lazy val Compile = Build(
    "Compile " + name, 
    () => Dependency.Graph.transitiveClosure(this, allUpstreamModules.map(SourceCompileTask(_))),
    this,
    "compile",
    "Compile module(s) " + allUpstreamModules.map(_.name).mkString(", ") + " after compiling any upstream modules"
  )

  lazy val TestCompile = Build(
    "Test Compile " + name, 
    () => Dependency.Graph.transitiveClosure(this, allUpstreamTestModules.map(TestCompileTask(_))),
    this,
    "testCompile",
    "Compile tests in module(s) " + allUpstreamTestModules.map(_.name).mkString(", ") + " after compiling any upstream source (and also tests in the case of upstreamTestProjects"
  )

  lazy val Test = {
    Build(
      "Test " + name, 
      () => Dependency.Graph.combine(allUpstreamModules.map(_.TestOnly.graph)),
      this,
      "test",
      "Run tests for module(s) " + allUpstreamModules.map(_.name).mkString(", ") + ". After any module fails, all currently running modules will continue till completion, however no tests for any downstream modules will be launched"
    )
  }
  
  lazy val TestClass = Build(
    "Test single class ",
    () => throw new Exception("Placeholder graph only - should never be called"),
    this,
    "testClass <class name>",
    "Exceutes a single test suite using the class path of " + name + ". Does IntelliJ style best match on (mandatory) provided class name - e.g. testClass(\"QuanTe\") instead of testClass(\"starling.quantity.QuantityTests\")"
  )

  lazy val TestFailedSuites = Build(
    "Run failing test suites for " + name + " and upstream ", 
    () => Dependency.Graph.combine(allUpstreamModules.map(_.TestFailedSuitesOnly.graph_())),
    this,
    "testFailedSuites",
    "Runs all failed tests in the module " + name + " and upstream"
  )

  lazy val PackageJars = Build(
    "Package jar(s) for " + name + " and upstream ", 
    () => Dependency.Graph(allUpstreamModules.map(PackageJarTask(_))),
    this,
    "pack",
    "Packages jars for " + name + " and upstream. Output jars will be " + allUpstreamModules.map(_.outputArtifact).mkString("\n\t")
  )
  
  lazy val Update = Build(
    "Update libraries for " + name,
    () => Dependency.Graph.combine(allUpstreamModules.map(_.UpdateOnly.graph_())),
    this,
    "update",
    "Update libraries for " + name
  )

  lazy val PublishLocal = Build(
    "Publish " + name + " locally",
    () => throw new Exception("Placeholder graph only - should never be called"),
    this,
    "publishLocal version",
    "Publish " + name + " to ~/.ivy2"
  )

  lazy val Publish = Build(
    "Publish " + name,
    () => throw new Exception("Placeholder graph only - should never be called"),
    this,
    "publish <version> <optional resolver>",
    "Publish " + name 
  )

  lazy val RunMain = Build(
    "Run single class",
    () => throw new Exception("Placeholder graph only - should never be called"),
    this,
    "runMain(className)(opts : String*)(args : String*)",
    ("""|Executes a single class using the class path of %s. Does IntelliJ style best match on (mandatory) provided class name - like testClass
        |opts are set as JVM -D args to the process runner
        |args are passed to the main method""" % name).stripMargin
  )

  lazy val Doc = Build(
    "Document " + name,
    () => Dependency.Graph.transitiveClosure(this, DocTask(this)),
    this, 
    "doc",
    "Document " + name + " aggregated with all upstream modules"
  )
  


  def clean = Clean.execute
  def cleanAll = CleanAll.execute
  def compile = Compile.execute
  def compileContinuously = continuously(Compile)
  def testCompile = TestCompile.execute
  def testCompileContinuously = continuously(TestCompile)
  def test = Test.execute
  def testClass(className : String, classNames : String*) = TestClass.copy(graph_ = () => Dependency.Graph.transitiveClosure(this, RunUnitTestsTask(this, className, classNames : _*))).execute
  def testClassContinuously(className : String) = continuously(TestClass.copy(graph_ = () => Dependency.Graph.transitiveClosure(this, RunUnitTestsTask(this, className))))
  def testFailedSuites = TestFailedSuites.execute
  def pack = PackageJars.execute
  def update = Update.execute
  def publishLocal(version : String) = {
    PublishLocal.copy(graph_ = () => Dependency.Graph.transitiveClosure(this, PublishLocalTask(this, version))).execute
  }
  def publish(version : String, resolver : String = props.defaultResolver()) = {
    Publish.copy(graph_ = () => Dependency.Graph.transitiveClosure(this, PublishTask(this, resolver, version))).execute
  }

  def runMain(className : String)(opts : String*)(args : String*) = {
    RunMain.copy(graph_ = () => Dependency.Graph.transitiveClosure(this, RunMainTask(this, className, opts.toList, args.toList))).execute
  }

  def doc = Doc.execute

  def builds = {
    val buildFields = this.getClass.getDeclaredFields.filter{f => classOf[maker.build.Build].isAssignableFrom(f.getType)}.map(_.getName)
    val helpText = """
      |Builds are directed graphs of tasks, such as Update, Clean, Compile, TestCompile and Test.
      |
      |By convention tasks are executed with the non-capitalized name - e.g. testCompile 
      |Each task has a help method for a fuller decription
      """.stripMargin
    println(helpText + buildFields.mkString("\n", "\t\n", ""))
  }

  // Some sugar
  def tcc = testCompileContinuously
  def stfe {
    props.ShowFailingTestException := ! props.ShowFailingTestException()
  }

  def continuously(bld : Build){

    var fileChanged = false
    var inotifyProcess : Option[(Process, Future[Int])] = None

    def launchInotifyWait{
      assert(!inotifyProcess.isDefined, "inotify may still be running")
      var runnable = new Runnable(){
        def run{
          try{
            val dirs = allUpstreamModules.flatMap{m => List(m.sourceDir, m.testSourceDir)}.filter(_.exists).map(_.getAbsolutePath)
            val args : List[String] = List(
              "inotifywait", "-q", "-r",
              "--exclude", "'.*\\.class'",
              "--event", "modify",
              "--event", "create",
              "--event", "move",
              "--event", "delete"
            ) ::: dirs
            val cmd = Command(
              CommandOutputHandler(),
              //CommandOutputHandler.NULL,
              Some(file(".")),
              args : _*
            )

            //block until a change happens
            val (proc, future) = cmd.execAsync
            inotifyProcess = Some((proc, future))
            println("\nWaiting for source file changes (press 'enter' to interrupt)")
            proc.waitFor

            fileChanged = true
            inotifyProcess = None
          } catch {
            case e : Throwable =>
              println("Error when running inotifywait, " + e)
          }
        }
      }
      val thread = new Thread(runnable)
      thread.start
    }
    def killInotifyProcess{
      val (proc, future) = inotifyProcess.get
      proc.destroy
      Futures.awaitAll(1000, future)
    }

    println(bld.execute)
    launchInotifyWait

    var running = true
    while (running) {

      Thread.sleep(bld.props.ContinuousTaskWaitInMillis())

      if (System.in.available > 0 && System.in.read == 10) {
        killInotifyProcess
        running = false
      } else if(fileChanged){
        fileChanged = false
        println(bld.execute)
        launchInotifyWait
      }
    }
  }


  lazy val isAccessibleScalaTestSuite : (String => Boolean) = {
    lazy val loader = new URLClassLoader(
      allUpstreamModules.flatMap{p => p.classpathJars.toSet + p.testOutputDir + p.outputDir}.map(_.toURI.toURL).toArray,
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
    dirsAndJars :::= props.compilerJars()
    // dirsAndJars ::= props.ProjectScalaCompilerJar()
    // dirsAndJars ::= props.ProjectScalaLibraryJar()
    val cp = Module.asClasspathStr(dirsAndJars)
    val cpFile : File = file(name + "-classpath.sh")
    writeToFile(cpFile, "export CLASSPATH=" + cp + "\n")
  }

  def constructorCodeAsString : String

  def writeMakerProjectDefinitionFile{
    val makerFile = file(rootAbsoluteFile, "Maker.scala")

    val buffer = new StringBuffer
    buffer.addLine(constructorCodeAsString)
    writeToFile(makerFile, buffer.toString)
  }

  def delete = recursiveDelete(rootAbsoluteFile)
}

