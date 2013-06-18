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
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.util.filter.FilterHelper
import org.apache.ivy.Ivy
import org.apache.ivy.core.retrieve.RetrieveOptions
import maker.task._
import maker.utils.Stopwatch
import maker.utils.maven.DependencyCacheLock


/**
 * Dependency management update task, fetch lib dependencies from specificied repositories
 */
case class UpdateTask(project : Project,
                      withSources : Boolean = true,
                      confs : Seq[String] = Array("default"),
                      omitIfNoIvyChanges : Boolean = false) extends Task {

  def name = "Update"
  def upstreamTasks : List[Task] = upstreamProjects.map(UpdateTask(_, withSources, confs, omitIfNoIvyChanges))

  private lazy val log = props.log
  private lazy val ivyFile = project.ivyFile

  def exec(results : List[TaskResult], sw : Stopwatch) : TaskResult = {
    def doUpdate() = {
      try {
        if (ivyFile.exists) {
          log.debug("confs : " + confs.mkString(", "))
          retrieveArtifacts(List("jar", "war", "bundle"), project.layout.managedLibDir.getPath + "/[organisation]-[artifact]-[revision](-[classifier]).[ext]")
          retrieveArtifacts(List("source"), project.layout.managedLibSourceDir.getPath + "/[organisation]-[artifact]-[revision](-[classifier]).[ext]")
          retrieveArtifacts(List("gz", "xml", "zip", "xll", "dna"), project.layout.managedResourceDir.getPath + "/[organisation]-[artifact](-[classifier]).[ext]")
        }
        else {
          log.info("Unable to update, no dependency definitions")
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

    checkForRequiredUpdate() match {
      case Some(moreRecentArtifact) ⇒ {
        log.debug("Updating, artifact %s is more recent than %s, %s > %s"
          .format(moreRecentArtifact, project.layout.ivyDepsFile, moreRecentArtifact.lastModified, project.layout.ivyDepsFile.lastModified))
        doUpdate()
      }
      case None if (omitIfNoIvyChanges) => {
        log.debug("Not updating, nothing to update (omit update is enabled)")
        TaskResult.success(this, sw)
      }
      case None ⇒ {
        log.debug("Updating, (omit update is disabled)")
        doUpdate()
      }
    }
  }

  // todo; this test needs revising since we'll not necessarily have ivy files
  private def checkForRequiredUpdate() =
    downloadedArtifacts.find(_.lastModified < project.layout.ivyDepsFile.lastModified)

  private def retrieveArtifacts(artifactTypes : List[String], pathTemplate : String) {
    log.info("Retreiving artifacts " + artifactTypes)

    val artifactFilter = FilterHelper.getArtifactTypeFilter(artifactTypes.toArray)
    val resolveOptions = new ResolveOptions().setConfs(confs.toArray)
      .setValidate(true)
      .setArtifactFilter(artifactFilter)

    DependencyCacheLock.synchronized {
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
}
