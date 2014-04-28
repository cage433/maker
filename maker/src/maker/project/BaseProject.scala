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

  protected def build(name : String, graph : Dependency.Graph) = Build(name, graph, props.NumberOfTaskThreads())
   
  lazy val Clean = build(
    buildName("Clean"),
    Dependency.Graph.transitiveClosure(this, allUpstreamModules.map(CleanTask(_)))
  )

  lazy val CleanAll = build(
    "Clean All " + name, 
    Dependency.Graph.transitiveClosure(this, allUpstreamModules.map(CleanTask(_, deleteManagedLibs = true)))
  )

  lazy val Compile = build(
    "Compile " + name, 
    Dependency.Graph.transitiveClosure(this, allUpstreamModules.map(SourceCompileTask))
  )

  lazy val TestCompile = build(
    "Test Compile " + name, 
    Dependency.Graph.transitiveClosure(this, allUpstreamTestModules.map(TestCompileTask))
  )

  def Test(verbose : Boolean) = build(
    "Test " + name, 
    Dependency.Graph.combine(allUpstreamModules.map(_.TestOnly(verbose).graph))
  )
  
  def TestSansCompile(verbose : Boolean) = build(
    "Test without compiling " + name,
    Dependency.Graph(allUpstreamModules.map(RunUnitTestsTask(_, verbose)))
  )

  def TestClass(className : String, verbose : Boolean) = build(
    "Test single class ",
    Dependency.Graph.transitiveClosure(this, RunUnitTestsTask(this, verbose, className))
  )

  def TestFailedSuites(verbose : Boolean) = build(
    "Run failing test suites for " + name + " and upstream ", 
    Dependency.Graph.combine(allUpstreamModules.map(_.TestFailedSuitesOnly(verbose).graph))
  )

  lazy val PackageJars = build(
    "Package jar(s) for " + name + " and upstream ", 
    Dependency.Graph(allUpstreamModules.map(PackageMainJarTask))
  )
  
  lazy val Update = build(
    "Update libraries for " + name,
    Dependency.Graph.combine(allUpstreamModules.map(_.UpdateOnly.graph))
  )

  def PublishLocal(version : String) = build(
    "Publish " + name + " locally",
    Dependency.Graph.transitiveClosure(this, PublishLocalTask(this, version))
  )


  def CreateDeploy(buildTests : Boolean) = build(
    "Create a deployment package of " + name + " in target-maker/deploy",
    Dependency.Graph.transitiveClosure(this, CreateDeployTask(this, buildTests))
  )


  def Publish(version : String, resolver : String = props.defaultResolver) = build(
    "Publish " + name,
    Dependency.Graph.transitiveClosure(this, PublishTask(this, resolver, version))
  )

  def RunMain(className : String)(opts : String*)(args : String*)  = build(
    "Run single class",
    Dependency.Graph.transitiveClosure(this, RunMainTask(this, className, opts.toList, args.toList))
  )

  lazy val Doc = build(
    "Document " + name,
    Dependency.Graph.transitiveClosure(this, DocTask(this))
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
    if (result.failed && props.ExecMode()){
      log.error(bld + " failed ")
      System.exit(-1)
    }
    BuildResult.lastResult.set(Some(result))
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

