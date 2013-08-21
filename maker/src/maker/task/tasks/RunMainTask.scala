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

import annotation.tailrec
import maker.project.Module
import maker.task.Task
import maker.utils.FileUtils._
import java.io.PrintWriter
import maker.utils.TeeToFileOutputStream
import maker.utils.os.CommandOutputHandler
import maker.utils.os.ScalaCommand
import maker.task._
import maker.utils.Stopwatch
import maker.MakerProps
import maker.task.compile.TestCompileTask
import maker.project.BaseProject


/**
 * run a class main in a separate JVM instance (but currently synchronously to maker repl)
 */
case class RunMainTask(baseProject : BaseProject, className : String, opts : List[String], mainArgs : List[String]) extends Task {
  def name = "Run Main"

  def upstreamTasks = baseProject.allUpstreamModules.map(TestCompileTask(_))


  val runLogFile = file("runlog.out")
  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    val props = baseProject.props
    val log = props.log
    log.info("running main in class " + className)

    val writer = new PrintWriter(new TeeToFileOutputStream(runLogFile))
    val optsToUse = List(
      "-Xmx" + props.TestProcessMemoryInMB() + "m", 
      "-XX:MaxPermSize=200m",
      "-Dlogback.configurationFile=" + "logback.xml"
    ) ::: opts
    val cmd = ScalaCommand(
      props,
      new CommandOutputHandler(Some(writer)),
      props.Java().getAbsolutePath,
      optsToUse,
      baseProject.testClasspath,
      className,
      "Running main in " + baseProject.name,
      mainArgs 
    )

    writeToFile(file("runcmd.sh"), "#!/bin/bash\n" + cmd.asString)
    log.info("Running, press ctrl-] to terminate running process...")

    val procHandle = cmd.execAsync()
    @tailrec
    def checkRunning(): TaskResult = {
      if (!procHandle._2.isSet) {
        Thread.sleep(1000)
        if (System.in.available > 0 && System.in.read == Task.termChar) {
          log.info("Terminating: " + className)
          procHandle._1.destroy()
          log.info("Terminated process for runMain of class : " + className)
          TaskResult.success(this, sw)
        }
        else checkRunning()
      }
      else {
        procHandle._2() match {
          case 0 => TaskResult.success(this, sw)
          case code => TaskResult.failure(this, sw, "Run Main failed in " + baseProject)
        }
      }
    }
    checkRunning()
  }
}
