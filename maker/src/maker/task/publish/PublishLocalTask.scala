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

package maker.task.publish

import maker.utils.FileUtils._
import org.apache.commons.io.FileUtils._
import maker.project._
import maker.task._
import maker.utils.Stopwatch
import maker.MakerProps
import maker.MakerProps
import java.io.IOException
import maker.utils.FileUtils
import maker.task.tasks.PackageJarTask
import maker.task.TaskContext


/**
 * publishes poms and packaged artifacts to the local filesystem at ~/.ivy2/maker-local - subject to change
 */
case class PublishLocalTask(baseProject : BaseProject, version : String) extends Task {
  private val configurations = List("default")

  def name = "Publish Local"

  def upstreamTasks = baseProject match {
    case _ : Project => baseProject.immediateUpstreamModules.map(PublishLocalTask(_, version))
    case m : Module => PackageJarTask(m) :: baseProject.immediateUpstreamModules.map(PublishLocalTask(_, version))
  }

  def exec(context : TaskContext) = {
    IvyLock.synchronized{
      doPublish(baseProject)
    }
  }
  
  private def doPublish(baseProject: BaseProject) = {

    FileUtils.writeToFile(baseProject.publishLocalPomFile, PomUtils.pomXml(baseProject, version))

    baseProject match {
      case _ : Project => 
      case m : Module => 
        copyFileToDirectory(m.outputArtifact, m.publishLocalJarDir)
    }
    TaskResult.success(this)
  }
}
