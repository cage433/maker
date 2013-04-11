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
import maker.utils.maven._
import maker.project._
import maker.task._
import maker.utils.Stopwatch
import scala.xml._
import maker.utils.ivy.IvyReader._
import maker.maven.PomWriter._

/**
 * makes per module poms and top level (reactor based) poms depending on the projects type,
 *   for the module and its upstream depeds
 */
case class MakePoms(
              project : Project,
              configurations : List[String] = List("default"),
              version : String,
              maybeParentProject : Option[TopLevelProject],
              includeUpstreamModuleTransitives : Boolean = true,  /// include upstream module direct dependencies for maven
              extractProps : Boolean = true                       /// extract properties into master pom
) extends Task {

  import maker.maven.PomWriter._

  val name = "Make Poms"

  def upstreamTasks = upstreamProjects.map(MakePoms(_, configurations, version, maybeParentProject))

  def exec(results : List[TaskResult], sw : Stopwatch) = {
    val props = project.props
    val moduleDef = project.moduleDef(Some(version), includeUpstreamModuleTransitives, false)
    val pomFile = file(project.rootAbsoluteFile, "pom.xml")
    val log = props.log
    log.info("make pom for project " + project.name)

    project match {
      // not 100% sure how this should work yet, so for now a top level project defines the master pom with pom based reactor modules
      case tlp : TopLevelProject => {
        val modules = tlp.allStrictlyUpstreamProjects
        val pomXml = makeTopLevelPom(tlp, moduleDef, modules, version)
        writeXmlToFile(pomFile, pomXml)
      }
      case _ => {
        project.ivyGeneratedFile match {
          case Some(ivyFile) => {
            val propertySubsDeps = moduleDef.projectDef.dependencyModules.map(d => d.updateVersion(propertyName(d.gav.groupAndArtifact)))
            //println("-- property sub deps --")
            //propertySubsDeps.foreach(println)
            // disabled, as this is too early-days to leave in yet...
            val updatedModuleDef = moduleDef //  moduleDef.withProjectDef(moduleDef.projectDef.withDeps(propertySubsDeps))

            writeXmlToFile(pomFile, makePom(moduleDef, project, maybeParentProject))
            //writePom(props, ivyFile, project.layout.ivySettingsFile, pomFile, configurations, updatedModuleDef, props.PomBuildTemplateFile())
          }
          case None => log.info("No Ivy file, skipping pom generation")
        }
      }
    }

    TaskResult.success(this, sw)
  }
}
