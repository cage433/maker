package maker.task.tasks

import maker.project.{Module, BaseProject}
import maker.utils.os.Command
import maker.task._
import maker.utils._
import maker.utils.RichIterable._
import maker.task.compile.TestCompileTask
import com.sun.org.apache.xpath.internal.operations.Bool
import maker.utils.RichString._
import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory
import maker.ConfigPimps
import java.sql.Time

case class RunUnitTestsTask(
  name : String, 
  baseProject : BaseProject, 
  classOrSuiteNames_ : Option[Iterable[String]],
  verbose: Boolean
)  
  extends Task 
  with ConfigPimps
{

  import baseProject.config
  override def failureHaltsTaskManager = false

  def upstreamTasks = baseProject.allUpstreamTestModules.map(TestCompileTask)

  def exec(rs : Iterable[TaskResult], sw : Stopwatch) : TaskResult = {

    // If no class names are passed in then they are found via reflection, so
    // compilation has to have taken place - hence class names can't be determined
    // at the point the task is created
    val classOrSuiteNames = (baseProject, classOrSuiteNames_) match {
      case (_, Some(cs)) => cs
      case (m : Module, None) => m.testClassNames()
      case _ => throw new RuntimeException("Can't run all tests against a top level project directly")
    }


    if (classOrSuiteNames.isEmpty) {
      return DefaultTaskResult(this, true, sw)
    }


    val suiteParameters : Seq[String] = classOrSuiteNames.map(List("-s", _)).flatten.toVector
    val systemPropertiesArguments = {
      var s = Map[String, String]()
      s += "scala.usejavacp" -> "true"
      s += "logback.configurationFile" -> config.unitTestLogbackConfigFile.getAbsolutePath
      s += "maker.test.output" -> baseProject.testOutputFile.toString
      s += "sbt.log.format" -> "=false"
      s.map{
        case (key, value) ⇒ "-D" + key + "=" + value
      }.toList
    }

    val memoryArguments = List(
      s"-Xmx${config.unitTestHeapSize}m"
    )

    baseProject.testOutputFile.delete

    val opts = config.debugFlags ::: memoryArguments ::: systemPropertiesArguments
 
    val testParameters : Seq[String] = {
      val consoleReporterArgs = if (verbose) List("-oF") else Nil
      consoleReporterArgs ::: List("-P", "-C", "maker.utils.MakerTestReporter") 
    }


    var cmd = Command.scalaCommand(
      classpath = baseProject.testClasspath + java.io.File.pathSeparator + config.testReporterJar,
      klass = "scala.tools.nsc.MainGenericRunner",
      opts = opts,
      args = "org.scalatest.tools.Runner" +: testParameters ++: suiteParameters
    )

    // Apache executor is noisy when exit is non-zero, so switch that off here.
    // Actual exit value is checked below.
    cmd = cmd.withExitValues(0, 1)

    if (baseProject.isTestProject)
      cmd = cmd.withNoOutput

    val res = cmd.run

    val results = MakerTestResults(baseProject.testOutputFile)

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
        message = Some("Test failed in " + baseProject + failingSuiteClassesText),
        testResults = results)
    }
    result
  }

}

object RunUnitTestsTask{
  import TaskResult.{COLUMN_WIDTHS, fmtNanos}
  lazy val logger = LoggerFactory.getLogger(this.getClass)
  def apply(baseProject : BaseProject, verbose : Boolean, classNamesOrAbbreviations : String*) : Task  = {
    def resolveClassName(cn : String) : List[String] = {
      if (cn.contains('.'))
        List(cn)
      else {
        val matchingTestClasses = StringUtils.bestIntellijMatches(
          cn,
          baseProject.testClassNames()
        )
        if (matchingTestClasses.isEmpty){
          logger.warn("No class matching " + cn + " found")
          Nil
        } else {
          if (matchingTestClasses.size > 1)
            logger.info("Multiple matches: " + matchingTestClasses.mkString(", ") + ", using " + matchingTestClasses.head)
          matchingTestClasses.take(1)
        }
      }
    }
    val classNames = classNamesOrAbbreviations.toList.flatMap(resolveClassName)
    if (classNames.isEmpty)
      RunUnitTestsTask(
        "run all tests",
        baseProject,
        None,
        verbose
      )
    else
      RunUnitTestsTask(
        "test class(es) " + classNames.mkString(", "),
        baseProject,
        Some(classNames),
        verbose
      )
  }

  def failingTests(module : Module, verbose : Boolean) : RunUnitTestsTask = {
    RunUnitTestsTask(
      "Failing tests",
      module,
      Some(MakerTestResults(module.testOutputFile).failingSuiteClasses),
      verbose
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
