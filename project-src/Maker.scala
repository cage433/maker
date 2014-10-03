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



package maker

import maker.project._
import maker.utils.FileUtils._
import java.io.File
import org.apache.commons.lang3.StringUtils
import maker.task.TaskResult._
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.Level._
import task.BuildResult
import maker.task.Task
import maker.utils.os.Command
import maker.utils.os.CommandOutputHandler
import scala.collection.mutable.{Map => MMap}
import maker.task.compile._
import scala.util.Properties

/**
 * Maker's own self-build definition,
 */
object Maker {

  private val props = MakerProps() ++ (
    "GroupId", "com.google.code.maker",
    "LogbackTestConfigFile", "logback-config/logback-unit-tests.xml"
  )

  def module(name : String, upstreamProjects : Module*) = {
    val root = file(name).asAbsoluteFile
    new Module(
      root,
      name,
      immediateUpstreamModules = upstreamProjects.toList,
      props = props
    ) with ClassicLayout
  }

  lazy val testReporter = module("test-reporter")
  lazy val utils = module("utils", testReporter)
  lazy val mkr = module("maker", utils)

  lazy val topLevel = new Project("top-level", file("."), List(mkr), props)

  // Used to disambiguate which maker is running in the repl.
  def pwd = println(Properties.userDir)

}
