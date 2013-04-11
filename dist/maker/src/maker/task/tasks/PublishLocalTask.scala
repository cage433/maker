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

import maker.utils.FileUtils._
import org.apache.commons.io.FileUtils._
import maker.maven._
import maker.project._
import maker.utils.Version
import maker.task._
import maker.utils.Stopwatch
import maker.MakerProps
import maker.MakerProps
import java.io.IOException
import maker.utils.maven.IvyLock


/**
 * publishes poms and packaged artifacts to the local filesystem at ~/.ivy2/maker-local - subject to change
 */
case class PublishLocalTask(project : Project, configurations : List[String] = List("default"), version : String) extends Task {
  def name = "Publish Local"

  def upstreamTasks = PackageJarTask(project) :: upstreamProjects.map(PublishLocalTask(_, configurations, version))

  def exec(results : List[TaskResult], sw : Stopwatch) = {
    IvyLock.synchronized{
      doPublish(project, results, sw)
    }
  }
  
  private def doPublish(project: Project, results : List[TaskResult], sw : Stopwatch) = {
    import maker.maven.PomWriter._
    val props = project.props
    val log = props.log
    val homeDir = props.HomeDir()
    val moduleDef = project.moduleDef(Some(version))
    val moduleLocal = file(homeDir, ".ivy2/maker-local/" + project.moduleDef().projectDef.moduleLibDef.gav.toPath)
    val moduleLocalPomDir = file(moduleLocal, "/poms/")
    moduleLocalPomDir.mkdirs
    val moduleJarDir = file(moduleLocal, "/jars/")
    moduleJarDir.mkdirs
    val pomFile = file(moduleLocalPomDir, "pom.xml")

    log.debug("PublishLocal for project " + project.name)

    project.ivyGeneratedFile match {
      case Some(ivyFile) => {
        writePom(props, ivyFile, project.layout.ivySettingsFile, pomFile, configurations, moduleDef, props.PomTemplateFile())
        try {
          copyFileToDirectory(project.outputArtifact, moduleJarDir)
          TaskResult.success(this, sw)
        }
        catch {
          case e : IOException ⇒ 
            val msg = "IOException when copying " + project.outputArtifact + " to " + moduleJarDir
            TaskResult.failure(this, sw, msg)
        }
      }
      case None => {
        log.warn("No Ivy file, can't create a pom")
        TaskResult.failure(this, sw, "No Ivy file, can't create a pom")
      }
    }
  }
}
