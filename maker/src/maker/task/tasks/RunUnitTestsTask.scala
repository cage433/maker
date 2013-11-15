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
import maker.utils.os.{ScalaCommand, WithJdbcCommand}
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


case class RunUnitTestsTask(name : String, baseProject : BaseProject, classOrSuiteNames_ : () ⇒ Iterable[String], verbose: Boolean)  extends Task {


  override def failureHaltsTaskManager = false
  val props = baseProject.props


  def upstreamTasks = baseProject.allUpstreamTestModules.map(TestCompileTask)

  def exec(rs : Iterable[TaskResult], sw : Stopwatch) : TaskResult = {

    val classOrSuiteNames = classOrSuiteNames_()

    if (classOrSuiteNames.isEmpty) {
      return TaskResult.success(this, sw)
    }

    val suiteParameters = classOrSuiteNames.map(List("-s", _)).flatten
    val systemProperties = (props.JavaSystemProperties.asMap + "scala.usejavacp" → "true").map{
      case (key, value) ⇒ "-D" + key + "=" + value
    }.toList
    baseProject.testOutputFile.delete
    val opts = List(
      "-Xmx" + props.TestProcessMemoryInMB() + "m", 
      "-XX:MaxPermSize=200m", 
      "-Dmaker.test.output=" + baseProject.testOutputFile,
      "-Dlogback.configurationFile=" + props.LogbackTestConfigFile(),
      "-Dsbt.log.format=false"
    ) ::: systemProperties
    val testParameters =
      if(!verbose)
        List("-P", "-C", "maker.utils.MakerTestReporter")
      else
        List("-o", "-C", "maker.utils.MakerTestReporter")
    val args = testParameters ++ suiteParameters
    val cmd = WithJdbcCommand(
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
    val result = if (results.failures.isEmpty){
      TaskResult.success(this, sw)
    } else {
      val failingSuiteClassesText = results.failingSuiteClasses.indented()
      TaskResult.failure(this, sw, "Test failed in " + baseProject + failingSuiteClassesText)
    }
    result.withInfo(results)
  }

}

object RunUnitTestsTask{
  def apply(module : Module, verbose : Boolean) : RunUnitTestsTask = {
    RunUnitTestsTask(
      module.name + " test all",
      module,
      () ⇒ module.testClassNames(),
      verbose
    )
  }
  
  def apply(baseProject : BaseProject, classNameOrAbbreviation : String, verbose : Boolean) : Task  = {
    def resolveClassName() = {
      if (classNameOrAbbreviation.contains('.'))
        List(classNameOrAbbreviation)
      else {
        val matchingTestClasses = StringUtils.bestIntellijMatches(
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
      resolveClassName,
      verbose
    )
  }


  def failingTests(module : Module, verbose : Boolean) : RunUnitTestsTask = {
    RunUnitTestsTask(
      "Failing tests",
      module,
      () ⇒ MakerTestResults(module.props, module.testOutputFile).failingSuiteClasses,
      verbose
    )
  }
}
