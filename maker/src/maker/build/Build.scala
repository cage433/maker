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

package maker.build

import maker.utils.Stopwatch
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedBlockingQueue
import maker.Props
import maker.utils.MakerLog
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory
import maker.utils.FileUtils._
import ch.qos.logback.classic.Level
import maker.task.compile.CompileTask
import maker.utils.Implicits.RichString._
import maker.utils.Implicits.RichIterable._
import maker.project.BaseProject
import java.util.Date
import java.util.concurrent.atomic.AtomicBoolean
import maker.task.test.TestResults
import maker.task.test.RunUnitTestsTask
import java.io.File
import maker.utils.ScreenUtils
import maker.project.Module
import maker.task.Task
import maker.task.TaskResult
import maker.akka.MakerActorSystem

case class Build(
  name : String,
  graph_ : () => Dependency.Graph,
  module : BaseProject,
  invokingMethod : String,
  helpText : String
) {

  def help{
    println(name + " - (Executed with method " + invokingMethod + ")\n")
    println(helpText)
  }

  override def toString = "Build " + name

  def toLongString = {
    val buf = new StringBuffer
    buf.append("Build\n  " + name + "\n")
    buf.append("  Graph:\n  " + graph.toString.split("\n").mkString("\n  ") + "\n")
    buf.toString
  }
  

  val props = module.props
  val log = props.log

  lazy val graph = graph_()

  def execute = {
    val buildManager = MakerActorSystem.buildManager(name, graph, props)
    BuildManager.execute(buildManager, Some((module, graph)))
  }

}
