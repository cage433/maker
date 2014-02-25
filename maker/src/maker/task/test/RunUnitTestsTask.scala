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
import maker.akka.RemoteActor
import maker.task.TaskContext
import akka.actor.{Props => AkkaProps}
import akka.pattern.Patterns
import akka.dispatch.Await
import akka.util.Duration
import java.util.concurrent.TimeUnit
import org.scalatest.events._


case class RunUnitTestsTask(
  name : String, 
  baseProject : BaseProject, 
  classOrSuiteNames_ : Option[Iterable[String]],
  runParallel : Boolean
)  extends Task {


  override def failureHaltsTaskManager = false
  val props = baseProject.props
  def upstreamTasks = baseProject.allUpstreamTestModules.map(TestCompileTask(_))

  def exec(context : TaskContext) : TaskResult = {
    val testEventCollector = context.actorSystem.actorOf(
      AkkaTestManager.props(baseProject.name),
      "TestManager-" + baseProject.nameSansSpaces
    )

    // If no class names are passed in then they are found via reflection, so 
    // compilation has to have taken place - hence class names can't be determined
    // at the point the task is created
    val classOrSuiteNames = (baseProject, classOrSuiteNames_) match {
      case (_, Some(cs)) => cs
      case (m : Module, None) => m.testClassNames()
      case _ => throw new RuntimeException("Can't run all tests against a top level project directly")
    }

    if (classOrSuiteNames.isEmpty) {
      return TaskResult.success(this)
    }

    val suiteParameters : List[String] = classOrSuiteNames.map(List("-s", _)).flatten.toList

    val consoleReporterArg = if (props.RunningInMakerTest()) Nil else List("-o")

    // -Dsbt.log.format - see note in bin/maker.sh as to why this is required
    val opts = List(
      "-Xmx" + props.TestProcessMemoryInMB() + "m", 
      "-XX:MaxPermSize=200m", 
      "-Dlogback.configurationFile=" + baseProject.logbackConfigFilePath,
      "-Dsbt.log.format=false",
      "-Dscala.usejavacp=true",
      "-Dmaker.test.module=" + baseProject.nameSansSpaces,
      props.MakerTestReporterClasspath.toCommandLine,
      props.RunningInMakerTest.toCommandLine("true")
    ) ::: RemoteActor.javaOpts(testEventCollector, context.actorSystem, "maker.scalatest.TestReporterActor")

    var args = List("-C", "maker.scalatest.AkkaTestReporter") ::: suiteParameters ::: consoleReporterArg
    if (runParallel)
      args = "-P" :: args
    val outputHandler = CommandOutputHandler().withSavedOutput
    val cmd = ScalaCommand(
      outputHandler,
      props.Java,
      opts,
      props.MakerTestReporterClasspath().absPath + ":" + baseProject.testClasspath,
      "maker.scalatest.RunTests",
      "Running tests in " + name,
      args 
    )
    val res = cmd.exec

    val fut = Patterns.ask(testEventCollector, AkkaTestManager.EVENTS, 10 * 1000)
    val events = Await.result(fut, Duration(100, TimeUnit.SECONDS)).asInstanceOf[List[Event]]
    val results = TestResults(Map[String, List[Event]](baseProject.name -> events))
    baseProject.lastTestResults.set(Some(results))
    val result = if (results.numFailedTests == 0){
      TaskResult.success(this, info = Some(results))
    } else {
      val failingSuiteClassesText = results.failedTestSuites.indented()
      TaskResult.failure(this, message = Some("Test failed in " + baseProject + failingSuiteClassesText), info = Some(results))
    }
    result

  }

}

object RunUnitTestsTask{
  def apply(baseProject : BaseProject, runParallel : Boolean, classNamesOrAbbreviations : String*) : Task  = {
    def resolveClassName(cn : String) : List[String] = {
      if (cn.contains('.'))
        List(cn)
      else {
        val matchingTestClasses = IntellijStringDistance.bestMatches(
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
        baseProject.name + " test all",
        baseProject,
        None,
        runParallel
      )
    else 
      RunUnitTestsTask(
        "Test class " + classNames.mkString(", "), 
        baseProject,
        Some(classNames),
        runParallel
      )
  }


  def failingTests(module : Module) : RunUnitTestsTask = {
    RunUnitTestsTask(
      "Failing tests",
      module,
      Some(module.lastTestResults.get.get.failedTestSuites),
      runParallel = true
    )
  }
}
