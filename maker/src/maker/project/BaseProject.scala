package maker.project

import maker.task.Dependency
import maker.task.BuildResult
import maker.task.Task
import maker.MakerProps
import maker.task.Build
import maker.task.compile.SourceCompileTask
import java.io.File
import maker.utils.FileUtils
import maker.utils.FileUtils._
import maker.task.compile.TestCompileTask
import maker.task.tasks._
import maker.utils.RichString._
import java.net.URLClassLoader
import java.lang.reflect.Modifier
import maker.utils.MakerTestResults
import maker.ivy.IvyUtils
import scala.xml.Elem

trait BaseProject {
  protected def root : File
  val rootAbsoluteFile = root.asAbsoluteFile
  lazy val testOutputFile = file(rootAbsoluteFile, "maker-test-output")
  def name : String
  def setUp(graph : Dependency.Graph) : Unit
  def tearDown(graph : Dependency.Graph, result : BuildResult) : Unit
  def extraUpstreamTasks(task : Task) : Set[Task] = Set.empty
  def extraDownstreamTasks(task : Task) : Set[Task] = Set.empty
  def props : MakerProps
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


  private def buildName(text : String) = {
    text + " " + getClass.getSimpleName.toLowerCase + " " + name
  }

  lazy val Clean = Build(
    buildName("Clean"),
    Dependency.Graph.transitiveClosure(this, allUpstreamModules.map(CleanTask(_))),
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
    Dependency.Graph.transitiveClosure(this, allUpstreamModules.map(CleanTask(_, deleteManagedLibs = true))),
    this,
    "cleanAll",
    "Same as clean except it also deletes managed libraries"
  )

  lazy val Compile = Build(
    "Compile " + name, 
    Dependency.Graph.transitiveClosure(this, allUpstreamModules.map(SourceCompileTask)),
    this,
    "compile",
    "Compile module(s) " + allUpstreamModules.map(_.name).mkString(", ") + " after compiling any upstream modules"
  )

  lazy val TestCompile = Build(
    "Test Compile " + name, 
    Dependency.Graph.transitiveClosure(this, allUpstreamTestModules.map(TestCompileTask)),
    this,
    "testCompile",
    "Compile tests in module(s) " + allUpstreamTestModules.map(_.name).mkString(", ") + " after compiling any upstream source (and also tests in the case of upstreamTestProjects"
  )

  def Test(verbose : Boolean) = Build(
    "Test " + name, 
    Dependency.Graph.combine(allUpstreamModules.map(_.TestOnly(verbose).graph)),
    this,
    "test (<verbose>)",
    "Run tests for module(s) " + allUpstreamModules.map(_.name).mkString(", ") + ". After any module fails, all currently running modules will continue till completion, however no tests for any downstream modules will be launched"
  )
  
  def TestSansCompile(verbose : Boolean) = Build(
    "Test without compiling " + name,
    Dependency.Graph(allUpstreamModules.map(RunUnitTestsTask(_, verbose))),
    this,
    "testNoCompile (<verbose>)",
    "Run tests for module(s) " + allUpstreamModules.map(_.name).mkString(", ") + ". No compilation at all is done before running tests"
  )

  def TestClass(className : String, verbose : Boolean) = Build(
    "Test single class ",
    Dependency.Graph.transitiveClosure(this, RunUnitTestsTask(this, verbose, className)),
    this,
    "testClass <class name> (<verbose>)",
    "Exceutes a single test suite using the class path of " + name + ". Does IntelliJ style best match on (mandatory) provided class name - e.g. testClass(\"QuanTe\") instead of testClass(\"starling.quantity.QuantityTests\")"
  )

  def TestFailedSuites(verbose : Boolean) = Build(
    "Run failing test suites for " + name + " and upstream ", 
    Dependency.Graph.combine(allUpstreamModules.map(_.TestFailedSuitesOnly(verbose).graph)),
    this,
    "testFailedSuites (<verbose>)",
    "Runs all failed tests in the module " + name + " and upstream"
  )

  lazy val PackageJars = Build(
    "Package jar(s) for " + name + " and upstream ", 
    Dependency.Graph(allUpstreamModules.map(PackageMainJarTask)),
    this,
    "pack",
    "Packages jars for " + name + " and upstream. Output jars will be " + allUpstreamModules.map(_.outputArtifact).mkString("\n\t")
  )
  
  lazy val Update = Build(
    "Update libraries for " + name,
    Dependency.Graph.combine(allUpstreamModules.map(_.UpdateOnly.graph)),
    this,
    "update",
    "Update libraries for " + name
  )

  def PublishLocal(version : String) = Build(
    "Publish " + name + " locally",
    Dependency.Graph.transitiveClosure(this, PublishLocalTask(this, version)),
    this,
    "publishLocal version",
    "Publish " + name + " to ~/.ivy2"
  )


  def CreateDeploy(buildTests : Boolean) = Build(
    "Create a deployment package of " + name + " in target-maker/deploy",
    Dependency.Graph.transitiveClosure(this, CreateDeployTask(this, buildTests)),
    this,
    "createDeploy <optional buildTests>",
    "Create deployment " + name + " to target-maker/deploy"
  )


  def Publish(version : String, resolver : String = props.defaultResolver) = Build(
    "Publish " + name,
    Dependency.Graph.transitiveClosure(this, PublishTask(this, resolver, version)),
    this,
    "publish <version> <optional resolver>",
    "Publish " + name 
  )

  def RunMain(className : String)(opts : String*)(args : String*)  = Build(
    "Run single class",
    Dependency.Graph.transitiveClosure(this, RunMainTask(this, className, opts.toList, args.toList)),
    this,
    "runMain(className)(opts : String*)(args : String*)",
    ("""|Executes a single class using the class path of %s. Does IntelliJ style best match on (mandatory) provided class name - like testClass
        |opts are set as JVM -D args to the process runner
        |args are passed to the main method""" % name).stripMargin
  )

  lazy val Doc = Build(
    "Document " + name,
    Dependency.Graph.transitiveClosure(this, DocTask(this)),
    this, 
    "doc",
    "Document " + name + " aggregated with all upstream modules"
  )
  


  def clean = execute(Clean)
  def cleanAll = execute(CleanAll)
  def compile = execute(Compile)
  def compileContinuously = continuously(Compile)
  def testCompile = execute(TestCompile)
  def testCompileContinuously = continuously(TestCompile)
  def test = execute(Test(false))
  def test(verbose : Boolean) = execute(Test(verbose))
  def testNoCompile = execute(TestSansCompile(false))
  def testNoCompile(verbose : Boolean) = execute(TestSansCompile(verbose))
  def testClass(className : String, verbose : Boolean = false) = execute(TestClass(className, verbose))
  def testClassContinuously(className : String) = {
    val build = TestClass(className, verbose = false)
    continuously(build)
  }
  def testFailedSuites = execute(TestFailedSuites(false))
  def testFailedSuites(verbose : Boolean) = execute(TestFailedSuites(verbose))
  def pack = execute(PackageJars)
  def update = execute(Update)

  def createDeploy(buildTests: Boolean = true) = execute(CreateDeploy(buildTests))

  def publishLocal(version : String) = execute(PublishLocal(version))
  def publish(version : String, resolver : String = props.defaultResolver()) = execute(Publish(version, resolver))


  def runMain(className : String)(opts : String*)(args : String*) = execute(RunMain(className)(opts : _*)(args : _*))

  def doc = execute(Doc)

  def builds = {
    val buildFields = this.getClass.getDeclaredFields.filter{f ⇒ classOf[maker.task.Build].isAssignableFrom(f.getType)}.map(_.getName)
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

  def execute(bld : Build) = {
    setUp(bld.graph)
    val result = bld.execute
    tearDown(bld.graph, result)
    result
  }

  def continuously(bld : Build){
    var lastTaskTime :Option[Long] = None

    def allSourceFiles : List[File] = allUpstreamModules.flatMap{
      proj ⇒ 
        proj.compilePhase.sourceFiles++ proj.testCompilePhase.sourceFiles
    }

    def sourceFileCount : Int = allSourceFiles.size
    var lastFileCount : Int = sourceFileCount 
    def sourceFileNames : String = allSourceFiles.map(_.getPath).sortWith(_<_).mkString(" ")
    var lastSourceFileNames : String = sourceFileNames

    def printWaitingMessage = println("\nWaiting for source file changes (press 'enter' to interrupt)")
    def rerunTask{
      println(bld.execute)
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
        case (_, _, m, n, _, _) if m != n ⇒ { rerunTask }                  // Source file has been added or deleted
        case (_, _, _, _, a, b) if a != b ⇒ { rerunTask }                  // Source file has been renamed
        case _ =>                                                    // Either no code yet or code has not changed
      }
    }
  }


  lazy val isAccessibleScalaTestSuite : (String ⇒ Boolean) = {
    lazy val loader = new URLClassLoader(
      allUpstreamModules.flatMap{p ⇒ p.classpathJars.toSet + p.testOutputDir + p.outputDir}.map(_.toURI.toURL).toArray,
      null
    )
    (className: String) ⇒  {
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
    writeToFile(makerFile, buffer.toString)
  }

  def delete = recursiveDelete(rootAbsoluteFile)
}

