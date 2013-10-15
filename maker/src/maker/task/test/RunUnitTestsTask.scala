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

package maker.task.test

import maker.project.Module
import java.lang.reflect.Modifier
import maker.utils.FileUtils
import maker.utils.os.{ScalaCommand, Command}
import maker.task.Task
import maker.utils.os.CommandOutputHandler
import maker.utils.FileUtils._
import maker.task.TaskResult
import maker.utils.Stopwatch
import maker.utils.Implicits.RichIterable._
import maker.Props
import maker.build.Dependency
import java.io.File
import maker.task.compile.TestCompileTask
import maker.task.compile.TestCompilePhase
import maker.task.compile.ModuleCompilePhase
import maker.project.BaseProject
import maker.task.compile.CompileTask
import maker.task.compile.TestCompileTask
import maker.utils.IntellijStringDistance
import maker.task.compile.SourceCompileTask
import maker.task.NullTask
import maker.scalatest.MakerTestReporter


case class RunUnitTestsTask(name : String, baseProject : BaseProject, reporter : MakerTestReporter, classOrSuiteNames_ : () => Iterable[String])  extends Task {


  override def failureHaltsTaskManager = false
  val props = baseProject.props
  def upstreamTasks = baseProject.allUpstreamTestModules.map(TestCompileTask(_))

  def exec(rs : Iterable[TaskResult], sw : Stopwatch) : TaskResult = {
    val log = props.log

    val classOrSuiteNames = classOrSuiteNames_()

    if (classOrSuiteNames.isEmpty) {
      return TaskResult.success(this, sw)
    }

    val suiteParameters = classOrSuiteNames.map(List("-s", _)).flatten
    val systemProperties = (props.JavaSystemProperties.asMap + ("scala.usejavacp" -> "true")).map{
      case (key, value) => "-D" + key + "=" + value
    }.toList

    val opts = List(
      "-Xmx" + props.TestProcessMemoryInMB() + "m", 
      "-XX:MaxPermSize=200m", 
      "-Dlogback.configurationFile=" + props.LogbackTestConfigFile(),
      "-Dsbt.log.format=false"
    ) ::: reporter.systemProperties ::: systemProperties
    val args = List("-P", "-C", reporter.scalatestReporterClass) ++ suiteParameters
    val outputHandler = CommandOutputHandler().withSavedOutput
    val cmd = ScalaCommand(
      props,
      outputHandler,
      props.Java,
      opts,
      baseProject.testClasspath + ":" + reporter.scalatestClasspah,
      "org.scalatest.tools.Runner", 
      "Running tests in " + name,
      args 
    )
    val res = cmd.exec
    val results = baseProject.testResults
    val result = if (results.failedTests.isEmpty){
      TaskResult.success(this, sw, info = Some(results))
    } else {
      val failingSuiteClassesText = results.failedTestSuites.indented()
      TaskResult.failure(this, sw, message = Some("Test failed in " + baseProject + failingSuiteClassesText), info = Some(results))
    }
    result
  }

}

object RunUnitTestsTask{
  def apply(module : Module, reporter : MakerTestReporter) : RunUnitTestsTask = {
    RunUnitTestsTask(
      module.name + " test all",
      module,
      reporter,
      () => module.testClassNames()
    )
  }
  
  def apply(baseProject : BaseProject, reporter : MakerTestReporter, classNameOrAbbreviation : String) : Task  = {
    def resolveClassName() = {
      if (classNameOrAbbreviation.contains('.'))
        List(classNameOrAbbreviation)
      else {
        val matchingTestClasses = IntellijStringDistance.bestMatches(
          classNameOrAbbreviation,
          baseProject.testClassNames()
        )
        if (matchingTestClasses.isEmpty){
          baseProject.log.warn("No class matching " + classNameOrAbbreviation + " found")
          Nil
        } else {
          if (matchingTestClasses.size > 1)
            baseProject.log.info("Multiple matches: " + matchingTestClasses.mkString(", ") + ", using " + matchingTestClasses.head)
          matchingTestClasses.take(1)
        }
      }
    }
    RunUnitTestsTask(
      "Test class " + classNameOrAbbreviation, 
      baseProject,
      reporter,
      resolveClassName
    )
  }


  def failingTests(module : Module) : RunUnitTestsTask = {
    RunUnitTestsTask(
      "Failing tests",
      module,
      module.makerTestReporter,
      () => TestResults(module).failedTestSuites
    )
  }
}
