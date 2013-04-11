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
import maker.utils.FileUtils._
import maker.task.Task
import maker.utils.{GAV, GroupAndArtifact}
import org.apache.commons.io.FileUtils._
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.util.filter.FilterHelper
import org.apache.ivy.Ivy
import org.apache.ivy.core.retrieve.RetrieveOptions
import xml.NodeSeq
import maker.task._
import maker.utils.Stopwatch
import maker.MakerProps
import maker.utils.maven.IvyLock

case class UpdateTask(project : Project, withSources : Boolean = true, confs : Seq[String] = Array("default")) extends Task {
  def name = "Update"
  lazy val log = props.log

  def upstreamTasks : List[Task] = upstreamProjects.map(UpdateTask(_, withSources, confs))

  private def ivyGeneratedFile = project.ivyGeneratedFile.getOrElse(throw new Exception("No ivy file for " + project))
  private def retrieveArtifacts(artifactTypes : List[String], pathTemplate : String){
    log.info("Retreiving artifacts " + artifactTypes)
    val artifactFilter = FilterHelper.getArtifactTypeFilter(artifactTypes.toArray)
    val resolveOptions = new ResolveOptions().setConfs(confs.toArray)
      .setValidate(true)
      .setArtifactFilter(artifactFilter)
    // #71 trying synchronized here as we seem to have intermittent update failure
    IvyLock.synchronized {
      val ivy = Ivy.newInstance
      val settings = ivy.getSettings
      props.IvyChecksums().map(checksums => settings.setVariable("ivy.checksums", checksums))
      settings.addAllVariables(System.getProperties)
      ivy.configure(project.layout.ivySettingsFile)

      ivy.configure(project.ivyGeneratedFile.get)
      val report = ivy.resolve(ivyGeneratedFile.toURI().toURL(), resolveOptions)
      val md = report.getModuleDescriptor
      ivy.retrieve(
        md.getModuleRevisionId(),
        pathTemplate,
        new RetrieveOptions()
          .setConfs(confs.toArray).setSync(true)
          .setArtifactFilter(artifactFilter))
    }
  }

  def exec(results : List[TaskResult], sw : Stopwatch) = {
    try {
      project.ivyGeneratedFile match {
        case Some(ivyGeneratedFile) => {
          log.debug("confs : " + confs.mkString(", "))
          retrieveArtifacts(List("jar", "war", "bundle"), project.layout.managedLibDir.getPath + "/[artifact]-[revision](-[classifier]).[ext]")
          retrieveArtifacts(List("source"), project.layout.managedLibSourceDir.getPath + "/[artifact]-[revision](-[classifier]).[ext]")
          retrieveArtifacts(List("gz", "xml", "zip", "xll", "dna"), project.layout.managedResourceDir.getPath + "/[artifact](-[classifier]).[ext]")

        }
        case None => {
          log.info("Nothing to update")
        }
      }
      TaskResult.success(this, sw)
    }
    catch {
      case e =>
        e.printStackTrace
        TaskResult.failure(this, sw, e)
    }
  }
}
