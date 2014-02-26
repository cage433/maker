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

package maker.task.update

import maker.project.Module
import maker.utils.FileUtils._
import maker.task.Task
import org.apache.commons.io.FileUtils._
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.util.filter.FilterHelper
import org.apache.ivy.Ivy
import org.apache.ivy.core.retrieve.RetrieveOptions
import xml.NodeSeq
import maker.task._
import maker.utils.Stopwatch
import maker.MakerProps
import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import maker.utils.os.Command
import maker.task.TaskContext

case class UpdateTask(module : Module) extends Task {
  def name = "Update " + module
  private val props = module.props
  
  def upstreamTasks : List[Task] = Nil

  def exec(context : TaskContext) : TaskResult = {
    // delete old resource files
    module.resources().map(_.resourceFile).groupBy(_.dirname).foreach{
      case (dir, expectedResourceFiles) => 
        val actualResourceFiles = dir.safeListFiles.map(_.asAbsoluteFile).toSet
        (actualResourceFiles -- expectedResourceFiles.map(_.asAbsoluteFile)).foreach(_.delete)
    }
    val missingResources = module.resources().filterNot(_.resourceFile.exists)
    // update any missing resources
    val (_, failures) = missingResources.partition(Exec(_).apply())
    failures match {
      case Nil => 
        TaskResult.success(this)
      case _ => 
        TaskResult.failure(this, message = Some("Failed to update resource(s) " + failures.mkString("\n\t", "\n\t", "\n\t")))
    }
  }

  private case class Exec(resource : Resource){
    def apply() : Boolean = {
      resource.update()
      resource.resourceFile.exists || resource.classifier == Some("sources")
    }
  }

}
