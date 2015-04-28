package maker.task.tasks

import maker.project.{Module, ProjectTrait}
import maker.utils.os.Command
import maker.task._
import maker.utils._
import maker.utils.RichIterable._
import maker.task.compile.{TestCompileTask, TestCompilePhase}
import com.sun.org.apache.xpath.internal.operations.Bool
import maker.utils.RichString._
import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory
import maker.{ConfigPimps, ScalaVersion}
import java.sql.Time

case class RunUnitTestsTask(
  name : String, 
  modules : Seq[Module],
  rootProject : ProjectTrait, 
  classOrSuiteNames_ : Option[Iterable[String]],
  scalaVersion : ScalaVersion
)  
  extends Task 
  with ConfigPimps
{

  import rootProject.config
  override def failureHaltsTaskManager = false

  def upstreamTasks = modules.map(TestCompileTask(rootProject, _, scalaVersion))

  def exec(rs : Iterable[TaskResult], sw : Stopwatch) : TaskResult = {

    // If no class names are passed in then they are found via reflection, so
    // compilation has to have taken place - hence class names can't be determined
    // at the point the task is created
    val classOrSuiteNames = classOrSuiteNames_.getOrElse(modules.flatMap(_.testClassNames(rootProject, scalaVersion)))

    if (classOrSuiteNames.isEmpty) {
      return DefaultTaskResult(this, true, sw)
    }


    val suiteParameters : Seq[String] = classOrSuiteNames.map(List("-s", _)).flatten.toVector
    val systemPropertiesArguments = {
      var s = Map[String, String]()
      s += "scala.usejavacp" -> "true"
      s += "logback.configurationFile" -> Option(System.getProperty("logback.configurationFile")).getOrElse(throw new Exception("No logback config defined"))
      s += "maker.test.output" -> rootProject.testOutputFile.toString
      s += "sbt.log.format" -> "=false"
      s.map{
        case (key, value) ⇒ "-D" + key + "=" + value
      }.toList
    }

    val memoryArguments = List(
      s"-Xmx${config.unitTestHeapSize}m"
    )

    rootProject.testOutputFile.delete

    val opts = config.debugFlags ::: memoryArguments ::: systemPropertiesArguments
 
    val testParameters : Seq[String] = rootProject.scalatestOutputParameters :: List("-P", "-C", "maker.utils.MakerTestReporter") 

    var cmd = Command.scalaCommand(
      classpath = rootProject.testRuntimeClasspath(scalaVersion),
      klass = "scala.tools.nsc.MainGenericRunner",
      opts = opts,
      args = "org.scalatest.tools.Runner" +: testParameters ++: suiteParameters
    )

    // Apache executor is noisy when exit is non-zero, so switch that off here.
    // Actual exit value is checked below.
    cmd = cmd.withExitValues(0, 1)

    val res = cmd.run

    val results = MakerTestResults(rootProject.testOutputFile)

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
        message = Some("Test failed in " + rootProject + failingSuiteClassesText),
        testResults = results)
    }
    result
  }

}

object RunUnitTestsTask{
  import TaskResult.{COLUMN_WIDTHS, fmtNanos}
  lazy val logger = LoggerFactory.getLogger(this.getClass)

  def failingTests(rootProject : ProjectTrait, module : Module, scalaVersion : ScalaVersion) : RunUnitTestsTask = {
    RunUnitTestsTask(
      "Failing tests",
      module :: Nil,
      rootProject,
      Some(MakerTestResults(module.testOutputFile).failingSuiteClasses),
      scalaVersion
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
