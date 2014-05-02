/*
 * Copyright (c) 2011-2012, Alex McGuire, Louis Botterill
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met: 
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package maker.task.tasks

import maker.project.Module
import maker.utils.os.ScalaCommand
import maker.task.Task
import maker.utils.os.CommandOutputHandler
import maker.task.TaskResult
import maker.utils.Stopwatch
import maker.utils.RichIterable._
import maker.project.BaseProject
import maker.utils.MakerTestResults
import maker.task.compile.TestCompileTask
import maker.utils.StringUtils
import com.sun.org.apache.xpath.internal.operations.Bool
import maker.utils.RichString._
import maker.task.DefaultTaskResult
import maker.utils.TestIdentifier
import maker.utils.TestFailure
import maker.utils.TableBuilder


case class RunUnitTestsTask(
  name : String, 
  baseProject : BaseProject, 
  classOrSuiteNames_ : Option[Iterable[String]],
  verbose: Boolean)  extends Task {

  def module = baseProject
  override def failureHaltsTaskManager = false
  val props = baseProject.props


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

    val consoleReporterArg = if (props.RunningInMakerTest()) Nil else List("-o")

    val suiteParameters = classOrSuiteNames.map(List("-s", _)).flatten
    val systemProperties = (props.JavaSystemProperties.asMap + ("scala.usejavacp" -> "true")).map{
      case (key, value) => "-D" + key + "=" + value
    }.toList
    baseProject.testOutputFile.delete
    val opts = List(
      "-Xmx" + props.TestProcessMemoryInMB() + "m", 
      "-XX:MaxPermSize=200m", 
      "-Dmaker.test.output=" + baseProject.testOutputFile,
      "-Dlogback.configurationFile=" + props.LogbackTestConfigFile(),
      "-Dsbt.log.format=false",
      props.RunningInMakerTest.toCommandLine("true")
    ) ::: systemProperties
    val testParameters =
      if(!verbose)
        List("-P", "-C", "maker.utils.MakerTestReporter")
      else
        List("-F", "-P", "-C", "maker.utils.MakerTestReporter")
    val args = testParameters ++ suiteParameters ++ consoleReporterArg
    val cmd = ScalaCommand(
      props,
      CommandOutputHandler(), 
      props.Java().getAbsolutePath, 
      opts,
      baseProject.testClasspath + java.io.File.pathSeparator + props.MakerTestReporterJar(),
      "org.scalatest.tools.Runner", 
      "Running tests in " + name,
      args 
    )
    val res = cmd.exec
    val results = MakerTestResults(baseProject.props, baseProject.testOutputFile)
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
  import Task.{COLUMN_WIDTHS, fmtNanos}
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
          baseProject.log.warn("No class matching " + cn + " found")
          Nil
        } else {
          if (matchingTestClasses.size > 1)
            baseProject.log.info("Multiple matches: " + matchingTestClasses.mkString(", ") + ", using " + matchingTestClasses.head)
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
      Some(MakerTestResults(module.props, module.testOutputFile).failingSuiteClasses),
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
    if (failures.nonEmpty){
      val tb = TableBuilder(
        "No  ",
        "Suite                   ",
        "Test                              ",
        "Message"
      )
      failures.zipWithIndex.foreach{
        case ((TestIdentifier(suite, _, test), TestFailure(message, _)), i) => 
          tb.addRow(i, suite, test, message)
      }
      println("Failing Tests".inBlue)
      println(tb)
      println("\nEnter BuildResult.lastResults for more information on failing tests\n\n".inRed)
    }
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
