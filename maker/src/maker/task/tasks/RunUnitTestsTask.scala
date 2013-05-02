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

import maker.project.Project
import java.lang.reflect.Modifier
import maker.utils.FileUtils
import maker.utils.os.{ScalaCommand, Command}
import maker.task.Task
import maker.utils.os.CommandOutputHandler
import maker.utils.FileUtils._
import maker.task.TaskResult
import maker.utils.Stopwatch
import maker.utils.RichIterable._
import maker.MakerProps
import maker.task.Dependency
import java.io.File
import maker.task.compile.TestCompileTask
import maker.task.compile.TestCompilePhase
import maker.task.compile.ProjectPhase

case class RunUnitTestsTask(project : Project, testClassNames: String*)  extends Task {

  val projectPhase = ProjectPhase(project, TestCompilePhase)

  def name = {
    if (testClassNames.isEmpty)
      "Test all"
    else
      "Test " + testClassNames.toList.formatted(4)
  }

  override def failureHaltsTaskManager = false


  private def suiteClassNames: List[String] = {
    val classNames = projectPhase.classFiles.map(_.className(projectPhase.outputDir)).filterNot(_.contains("$"))
    classNames.filter(project.isAccessibleScalaTestSuite).toList
  }

  def upstreamTasks = TestCompileTask(project) :: Nil

  def exec(rs : List[TaskResult], sw : Stopwatch) : TaskResult = {
    val props = project.props
    val log = props.log

    val classOrSuiteNames : List[String] = if (testClassNames.nonEmpty)
      testClassNames.toList
    else
      suiteClassNames

    if (classOrSuiteNames.isEmpty) {
      log.info(project.name + " has no tests")
      return TaskResult.success(this, sw)
    }

    val suiteParameters = classOrSuiteNames.map(List("-s", _)).flatten
    val systemProperties = (props.JavaSystemProperties.asMap + "scala.usejavacp" → "true").map{
      case (key, value) ⇒ "-D" + key + "=" + value
    }.toList
    project.testOutputFile.delete
    val opts = List(
      "-Xmx" + props.TestProcessMemoryInMB() + "m", 
      "-XX:MaxPermSize=500m", 
      "-Dmaker.test.output=" + project.testOutputFile, 
      props.ShowTestProgress.toCommandLine,
      props.ProjectTestLogbackConfigFile.toCommandLine,
      "-Dsbt.log.format=false"
    ) ::: systemProperties
    val args = List("-P", "-C", "maker.utils.MakerTestReporter", "-R", projectPhase.outputDir.getAbsolutePath) ::: suiteParameters
    val cmd = ScalaCommand(
      props,
      CommandOutputHandler(), 
      props.Java().getAbsolutePath, 
      opts,
      projectPhase.compilationClasspath + ":" + props.MakerTestReporterJar() + ":" + props.ScalaCompilerJar(),
      "org.scalatest.tools.Runner", 
      "Running tests in " + project.name,
      args 
    )
    val res = cmd.exec
    val results = project.testResultsOnly
    val result = if (results.failures.isEmpty){
      TaskResult.success(this, sw)
    } else {
      val failingSuiteClassesText = results.failingSuiteClasses.indented()
      TaskResult.failure(this, sw, "Test failed in " + project + failingSuiteClassesText)
    }
    result.withInfo(project.testResultsOnly)
  }

}
