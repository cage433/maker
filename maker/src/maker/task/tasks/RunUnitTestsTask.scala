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
      return TaskResult.success(this, sw)
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
      TaskResult.success(this, sw)
    } else if (results.failures.isEmpty){
      TaskResult.failure(this, sw, message = Some("scalatest process bombed out. $? = " + res))
    } else {
      val failingSuiteClassesText = results.failingSuiteClasses.indented()
      TaskResult.failure(this, sw, message = Some("Test failed in " + baseProject + failingSuiteClassesText))
    }
    result.copy(info = Some(results))
  }

}

object RunUnitTestsTask{
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

  //def apply(module : Module, verbose : Boolean) : RunUnitTestsTask = {
    //RunUnitTestsTask(
      //module.name + " test all",
      //module,
      //Some(module.testClassNames()),
      //verbose
      //)
    //}
  

  def failingTests(module : Module, verbose : Boolean) : RunUnitTestsTask = {
    RunUnitTestsTask(
      "Failing tests",
      module,
      Some(MakerTestResults(module.props, module.testOutputFile).failingSuiteClasses),
      verbose
    )
  }
}
