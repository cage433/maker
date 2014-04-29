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

import maker.project._
import java.util.Date
import org.apache.ivy.core.publish.PublishOptions
import org.apache.ivy.util.filter.FilterHelper
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.Ivy
import maker.task._
import maker.utils.Stopwatch
import maker.MakerProps
import maker.utils.maven.IvyLock


case class PublishTask(baseProject : BaseProject, resolverName : String, version : String) extends Task {

  def name = "Publish"
  def module = baseProject
  def upstreamTasks = PublishLocalTask(baseProject, version = version) :: baseProject.immediateUpstreamModules.map(PublishTask(_, resolverName, version))
  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    IvyLock.synchronized{
      doPublish(baseProject, results, sw)
    }
  }

  private def doPublish(baseProject: BaseProject, results : Iterable[TaskResult], sw : Stopwatch) = {

    val props : MakerProps = baseProject.props

    val ivyFile = baseProject.ivyFile
    try {
      val confs = Array[String]("default")
      val artifactFilter = FilterHelper.getArtifactTypeFilter(Array[String]("xml", "jar", "bundle", "source"))
      val resolveOptions = new ResolveOptions().setConfs(confs)
        .setValidate(true)
        .setArtifactFilter(artifactFilter)
      val ivy = Ivy.newInstance
      val settings = ivy.getSettings

      settings.addAllVariables(System.getProperties)
      ivy.configure(baseProject.ivySettingsFile)

      val report = ivy.resolve(ivyFile.toURI().toURL(), resolveOptions)
      val md = report.getModuleDescriptor

      import scala.collection.JavaConversions._

      val po = new PublishOptions()
                    .setConfs(confs).setOverwrite(true)
                    .setPubrevision(version)
                    .setPubdate(new Date())

      val srcArtifactPattern = List(
        baseProject.publishLocalDir.getAbsolutePath + "/[type]s/pom.xml",
        baseProject.publishLocalDir.getAbsolutePath + "/[type]s/" + baseProject.artifactId + ".jar")


      ivy.publish(
        md.getModuleRevisionId(),
        srcArtifactPattern,
        resolverName,
        po)

      TaskResult.success(this, sw)
    }
    catch {
      case e =>
        e.printStackTrace
        TaskResult.failure(this, sw, e)
    }
  }
}
