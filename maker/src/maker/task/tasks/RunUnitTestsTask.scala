package maker.task.tasks

import maker.project.{Module, ProjectTrait}
import maker.utils.os.Command
import maker.task._
import maker.utils._
import maker.utils.FileUtils._
import maker.utils.RichIterable._
import maker.task.compile._
import com.sun.org.apache.xpath.internal.operations.Bool
import maker.utils.RichString._
import maker.{ScalaVersion, Log}
import java.sql.Time
import java.io.File
import java.lang.reflect.Modifier

case class RunUnitTestsTask(
  name : String, 
  module : Module,
  rootProject : ProjectTrait, 
  classNamesOrPhase: Either[Seq[String], TestPhase],
  lastCompilationTimeFilter : Option[Long]
)  
  extends Task with Log with FileUtils
{

  /**
    * Used to communicate with the test reporter,
    * in particular to get it to report on long running
    * or hanging tests
    */
  private val testReporterMessagingDir = {
    file(rootProject.rootAbsoluteFile, ".maker-test-reporter-messaging").asNewDirectory
  }

  override def failureHaltsTaskManager = false

  def upstreamTasks = UpdateTask(rootProject) +: CompilePhase.TEST_PHASES.map{p => CompileTask(rootProject, module, p)}

  def exec(rs : Iterable[TaskResult], sw : Stopwatch) : TaskResult = {

    // If no class names are passed in then they are found via reflection, so
    // compilation has to have taken place - hence class names can't be determined
    // at the point the task is created
    val classOrSuiteNames = classNamesOrPhase match {
      case Left(classOrSuiteNames) => 
        classOrSuiteNames
      case Right(testPhase) => 
        val isTestSuite = RunUnitTestsTask.isAccessibleScalaTestSuite(module)
        def filterByCompilationTime(classFiles: Seq[File]) = lastCompilationTimeFilter match {
          case Some(time) => classFiles.filter(_.lastModified >= time)
          case None => classFiles
        }
        val classNames = filterByCompilationTime(module.classFiles(testPhase)).map{
          classFile => 
            classFile.className(module.classDirectory(testPhase))
        }.filterNot(_.contains("$"))
        classNames.filter(isTestSuite)
    }

    if (classOrSuiteNames.isEmpty) {
      return DefaultTaskResult(this, true, sw)
    }


    val suiteParameters : Seq[String] = classOrSuiteNames.map(List("-s", _)).flatten.toVector
    val testOutputFile = RunUnitTestsTask.testOutputFile(module)
    val systemPropertiesArguments = {
      var s = Map[String, String]()
      s += "scala.usejavacp" -> "true"
      s += "logback.configurationFile" -> Option(System.getProperty("logback.configurationFile")).getOrElse(throw new Exception("No logback config defined"))
      s += "maker.test.output" -> testOutputFile.toString
      s += "sbt.log.format" -> "=false"
      s.map{
        case (key, value) ⇒ "-D" + key + "=" + value
      }.toList ++: rootProject.extraTestSystemProperties
    }

    val memoryArguments = List(
      s"-Xmx${rootProject.unitTestHeapSize}m"
    )

    testOutputFile.delete

    val opts : Seq[String] = rootProject.remoteDebuggingOption ++: memoryArguments ++: systemPropertiesArguments
 
    val testParameters : Seq[String] = rootProject.scalatestOutputParameters :: List("-P", "-C", "maker.utils.MakerTestReporter") 

    var cmd = {
      val args : Seq[String] = List(
        rootProject.javaExecutable.getAbsolutePath, 
        "-classpath",
        module.runtimeClasspath(CompilePhase.PHASES)) ++:
        (opts :+ "org.scalatest.tools.Runner") ++:
        testParameters ++: suiteParameters
      Command(args : _*).withOutputTo(System.err)
    }

    // Apache executor is noisy when exit is non-zero, so switch that off here.
    // Actual exit value is checked below.
    cmd = cmd.withExitValues(0, 1)

    val res = cmd.run

    val results = MakerTestResults(testOutputFile)

    val result = if (res == 0 && results.failures.isEmpty){
      RunUnitTestsTaskResult(this, succeeded = true, stopwatch = sw, testResults = results)
    } else if (results.failures.isEmpty){
      RunUnitTestsTaskResult(
        this, succeeded = false, stopwatch = sw, 
        message = Some("scalatest process bombed out. $? = " + res),
        testResults = results)
    } else {
      val failingSuiteClassesText = results.failingSuiteClasses.indented()
      RunUnitTestsTaskResult(
        this, succeeded = false, stopwatch = sw, 
        message = Some("Test failed in " + module + failingSuiteClassesText),
        testResults = results)
    }
    result
  }

}

object RunUnitTestsTask extends Log {
  import TaskResult.{COLUMN_WIDTHS, fmtNanos}

  def failingTests(rootProject : ProjectTrait, module : Module) : RunUnitTestsTask = {
    RunUnitTestsTask(
      "Failing tests",
      module,
      rootProject,
      Left(MakerTestResults(testOutputFile(module)).failingSuiteClasses),
      lastCompilationTimeFilter = None
    )
  }

  def testResults(taskResults : List[TaskResult]) : MakerTestResults = {
    taskResults.collect{
      case r : RunUnitTestsTaskResult => r.testResults
    }.fold(MakerTestResults())(_++_)
  }

  def reportOnFailingTests(taskResults : List[TaskResult]){
    val mergedTestResults = testResults(taskResults)
    val failures : List[(TestIdentifier, TestFailure)] = mergedTestResults.failures
    FailingTests.setFailures(failures)
    FailingTests.report
  }

  def reportOnSlowTests(taskResults : List[TaskResult]){
    val testResults_ = testResults(taskResults)
    if (testResults_.tests.isEmpty)
      return

    println("\nSlowest 5 test suites".inBlue)
    val suiteTable = TableBuilder(
      "Suite".padRight(COLUMN_WIDTHS(0)), 
      "Num Tests".padRight(COLUMN_WIDTHS(1)),
      "CPU Time".padRight(COLUMN_WIDTHS(2)),
      "Clock Time"
    )
    testResults_.orderedSuiteTimes.take(5).foreach{
      case (suite, clockTime, cpuTime, numTests) =>
        suiteTable.addRow(
          suite,
          numTests,
          fmtNanos(cpuTime),
          fmtNanos(clockTime)
        )
    }
    println(suiteTable.toString)

    println("\nSlowest 5 tests".inBlue)
    val testTable = TableBuilder(
      "Suite".padRight(COLUMN_WIDTHS(0)), 
      "Test".padRight(COLUMN_WIDTHS(1) + COLUMN_WIDTHS(2)), 
      "Clock Time")
    testResults_.testsOrderedByTime.take(5).foreach{
      case (TestIdentifier(suite, _, test), clockTime) => 
        testTable.addRow(
          suite,
          test,
          fmtNanos(clockTime)
        )
    }
    println(testTable.toString)
  }

  def testOutputFile(project: ProjectTrait): File = {
    file(project.rootAbsoluteFile, "maker-test-output")
  }

  def isAccessibleScalaTestSuite(rootProject : ProjectTrait) : (String => Boolean) = {
    val loader = rootProject.testClasspathLoader

    className : String => {

      def loadClass(className : String) = {
        try {
          loader.loadClass(className)
        } catch {
          case e : ClassNotFoundException => 
            println(
              s"""Couldn't load class $className in project $this, 
                  classpath was ${rootProject.runtimeClasspathComponents(TestCompilePhase :: Nil).mkString("\n\t", "\n\t", "\n")}""")
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

}

case class RunUnitTestsTaskResult(
  task : RunUnitTestsTask, 
  succeeded : Boolean, 
  stopwatch : Stopwatch,
  testResults : MakerTestResults,
  override val message : Option[String] = None, 
  override val exception : Option[Throwable] = None
) extends TaskResult{
}
