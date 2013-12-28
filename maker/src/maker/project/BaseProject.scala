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

  /* To override the logback config file would require a system property, which
     seems odd given logback.configurationFile is the standard system property
     for this. No current reason to override so will go with convention instead
  */
  def logbackConfigFilePath = {
    val f = file(props.root, "logback.xml")
    require(f.exists, f + " doesn't exist")
    f.getAbsolutePath
  }

  def toIvyExclude : Elem = <exclude org={groupId} module={artifactId} />
  def publishLocalDir = file(props.PublishLocalRootDir(), groupId, artifactId).makeDirs
  def publishLocalPomFile = file(file(publishLocalDir, "/poms/").makeDir, "pom.xml")

  def ivyFile = IvyUtils.generateIvyFile(this)
  def projectTypeName = this.getClass.getSimpleName // 'Module' or 'Project# 

  def testClasspath = Module.asClasspathStr(
    allUpstreamModules.flatMap(_.testCompilePhase.classpathDirectoriesAndJars)
  )

  def testClassNames() : Iterable[String]

  def docOutputDir : File

  val lastTestResults : AtomicReference[Option[TestResults]] = new AtomicReference(None)

  private def buildName(text : String) = {
    text + " " + getClass.getSimpleName.toLowerCase + " " + name
  }

  protected def build(name : String, graph : Dependency.Graph) = Build(name, graph, props.NumberOfTaskThreads())
  lazy val Clean = build(
    buildName("Clean"),
    Dependency.Graph.transitiveClosure(this, allUpstreamModules.map(CleanTask(_))))

  lazy val CleanAll = build(
    "Clean All " + name, 
    Dependency.Graph.transitiveClosure(this, allUpstreamModules.map(CleanTask(_, deleteManagedLibs = true))))

  lazy val Compile = build(
    "Compile " + name, 
    Dependency.Graph.transitiveClosure(this, allUpstreamModules.map(SourceCompileTask(_))))

  lazy val TestCompile = build(
    "Test Compile " + name, 
    Dependency.Graph.transitiveClosure(this, allUpstreamTestModules.map(TestCompileTask(_))))

  lazy val Test = build(
      "Test " + name, 
      Dependency.Graph.combine(allUpstreamModules.map(_.TestOnly.graph)))
  
  def TestClass(className : String, moreClassNames : String*) = build(
    "Test single class ",
    Dependency.Graph.transitiveClosure(this, RunUnitTestsTask(this, className, moreClassNames : _*)))

  def TestFailedSuites() = build(
    "Run failing test suites for " + name + " and upstream ", 
    Dependency.Graph.combine(allUpstreamModules.map(_.TestFailedSuitesOnly().graph)))

  lazy val PackageJars = build(
    "Package jar(s) for " + name + " and upstream ", 
    Dependency.Graph(allUpstreamModules.map(PackageJarTask(_))))
  
  lazy val Update = build(
    "Update libraries for " + name,
    Dependency.Graph.combine(allUpstreamModules.map(_.UpdateOnly.graph)))

  def PublishLocal(version : String) = build(
    "Publish " + name + " locally",
    Dependency.Graph.transitiveClosure(this, PublishLocalTask(this, version)))

  def Publish(version : String, resolver : String = props.defaultResolver()) = build(
    "Publish " + name,
    Dependency.Graph.transitiveClosure(this, PublishTask(this, resolver, version)))

  def RunMain(className : String)(opts : String*)(args : String*)  = build(
    "Run single class",
    Dependency.Graph.transitiveClosure(this, RunMainTask(this, className, opts.toList, args.toList)))

  lazy val Doc = build(
    "Document " + name,
    Dependency.Graph.transitiveClosure(this, DocTask(this)))
  


  def clean = execute(Clean)
  def cleanAll = execute(CleanAll)
  def compile = execute(Compile)
  def compileContinuously = continuously(Compile)
  def testCompile = execute(TestCompile)
  def testCompileContinuously = continuously(TestCompile)
  def test = execute(Test)
  def testClass(className : String, moreClassNames : String*) = execute(TestClass(className, moreClassNames : _*))
  def testClassContinuously(className : String) = continuously(TestClass(className))
  def testFailedSuites = execute(TestFailedSuites())
  def pack = execute(PackageJars)
  def update = execute(Update)
  def publishLocal(version : String) = execute(PublishLocal(version))
  def publish(version : String, resolver : String = props.defaultResolver()) = execute(Publish(version, resolver))

  def runMain(className : String)(opts : String*)(args : String*) = execute(RunMain(className)(opts : _*)(args : _*))

  def doc = execute(Doc)

  // Some sugar
  def tcc = testCompileContinuously
  def stfe {
    props.ShowFailingTestException := ! props.ShowFailingTestException()
  }

  def execute(bld : Build) = {
    setUp(bld.graph)
    val result = bld.execute
    tearDown(bld.graph, result)
    result
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

    println(execute(bld))
    launchInotifyWait

    var running = true
    while (running) {

      Thread.sleep(props.ContinuousTaskWaitInMillis())

      if (System.in.available > 0 && System.in.read == 10) {
        killInotifyProcess
        running = false
      } else if(fileChanged){
        fileChanged = false
        println(execute(bld))
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

