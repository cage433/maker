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

package maker.task

import maker.project.Project
import tasks._
import maker.utils.RichString._
import maker.task.TaskResult._
import java.io.File
import scalaz.Scalaz._
import maker.utils.Stopwatch
import maker.MakerProps

trait Task {
  def name : String
  override def toString = name + " " + project
  def toShortString = toString
  def exec(results : List[TaskResult] = Nil, sw : Stopwatch) : TaskResult
  def failureHaltsTaskManager : Boolean = true

  /**
   * Tasks that normally need to run BEFORE this one does 
   */
  def upstreamTasks : List[Task] 
  def project : Project
  def props : MakerProps = project.props
  def extraUpstreamTasks = project.extraUpstreamTasks(this)
  def extraDownstreamTasks = project.extraDownstreamTasks(this)
  def upstreamProjects = project.upstreamProjects
  def upstreamTestProjects = project.upstreamTestProjects
  def numberOfSourceFiles = 0
}

object NullTask extends Task{
  def name = "Null Task"
  def exec(results : List[TaskResult] = Nil, sw : Stopwatch) : TaskResult = TaskResult.success(this, sw)
  def upstreamTasks = Nil
  def project : Project = throw new Exception("Null task has no project")
}

object Task {
  val termSym = "ctrl-]"
  val termChar = 29 // ctrl-]
}
