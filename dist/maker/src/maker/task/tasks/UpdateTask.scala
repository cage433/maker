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

case class UpdateTask(project : Project, withSources : Boolean = true, confs : Seq[String] = Array("default"), omitIfNoIvyChanges : Boolean = false) extends Task {
  def name = "Update"
  lazy val log = props.log
  lazy val ivyFile = project.layout.ivyFile

  def upstreamTasks : List[Task] = upstreamProjects.map(UpdateTask(_, withSources, confs, omitIfNoIvyChanges))

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

      ivy.configure(ivyFile)
      val report = ivy.resolve(ivyFile.toURI().toURL(), resolveOptions)
      val md = report.getModuleDescriptor
      ivy.retrieve(
        md.getModuleRevisionId(),
        pathTemplate,
        new RetrieveOptions()
          .setConfs(confs.toArray).setSync(true)
          .setArtifactFilter(artifactFilter))
    }
  }

  private def downloadedArtifacts = List(
    project.layout.managedLibDir, 
    project.layout.managedLibSourceDir, 
    project.layout.managedResourceDir
  ).flatMap(allFiles).filter(_.exists).filterNot(_.isDirectory)

  def exec(results : List[TaskResult], sw : Stopwatch) : TaskResult = {
    if (omitIfNoIvyChanges){
      downloadedArtifacts.find(_.lastModified < project.layout.ivyFile.lastModified) match {
        case Some(moreRecentArtifact) ⇒ {
          log.debug("Updating as artifact " + moreRecentArtifact + " is more recent than " + project.layout.ivyFile + ". " + moreRecentArtifact.lastModified + " vs " + project.layout.ivyFile.lastModified)
        }
        case None ⇒ {
          return TaskResult.success(this, sw)
        }
      }
    }


    try {
      if (ivyFile.exists){
        log.debug("confs : " + confs.mkString(", "))
        retrieveArtifacts(List("jar", "war", "bundle"), project.layout.managedLibDir.getPath + "/[organisation]-[artifact]-[revision](-[classifier]).[ext]")
        retrieveArtifacts(List("source"), project.layout.managedLibSourceDir.getPath + "/[organisation]-[artifact]-[revision](-[classifier]).[ext]")
        retrieveArtifacts(List("gz", "xml", "zip", "xll", "dna"), project.layout.managedResourceDir.getPath + "/[organisation]-[artifact](-[classifier]).[ext]")

      } else {
        log.info("Nothing to update")
      }
      downloadedArtifacts.foreach(_.touch) // touch so we don't update again until we amend ivy.xml
      TaskResult.success(this, sw)
    }
    catch {
      case e =>
        e.printStackTrace
        TaskResult.failure(this, sw, e)
    }
  }
}
