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

import maker.project.Module
import maker.utils.FileUtils._
import java.io.PrintWriter
import maker.utils.TeeToFileOutputStream
import maker.utils.os.{ScalaDocCmd, CommandOutputHandler}
import maker.task.Task
import maker.task._
import maker.utils.Stopwatch
import maker.MakerProps
import maker.task.compile.SourceCompileTask
import maker.task.compile._
import maker.project.BaseProject


/** Doc generation task - produces scaladocs from module sources
  *
  * Outputs scala-docs per module in the "docs" sub-dir of the module target output dir
  */
case class DocTask(baseProject : BaseProject) extends Task {
  
  def name = "Doc " + baseProject.name
  def upstreamTasks = baseProject.allUpstreamModules.map(SourceCompileTask(_)).toList
  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    val props = baseProject.props
    val log = props.log

    val runDocLogFile = file("rundoc.out")

    log.info("running scala-doc gen for module " + baseProject)

    val writer = new PrintWriter(new TeeToFileOutputStream(runDocLogFile))

    val projects = baseProject.allUpstreamModules
    val (classpath, inputFiles) = (
      projects.map(_.compilePhase.compilationClasspath).mkString(":"),
      projects.flatMap(_.compilePhase.sourceFiles))


    val docDir = baseProject.docOutputDir
    if (!docDir.exists || lastModifiedFileTime(inputFiles).getOrElse(0L) > lastModifiedFileTime(List(docDir)).getOrElse(0L)) {
      log.debug("generating doc for module " + baseProject.toString)
      if (!docDir.exists) docDir.mkdirs else docDir.deleteAll

      // make a separate opts file as the args can get too big for a single command
      val optsFile = file(docDir, "docopts")
      writeToFile(optsFile, "-classpath " + classpath + " " + inputFiles.mkString(" "))

      val scalaToolsClasspath = baseProject.props.ProjectScalaCompilerJar().getAbsolutePath + ":" + baseProject.props.ProjectScalaLibraryJar().getAbsolutePath

      val cmd = ScalaDocCmd(
        props,
        new CommandOutputHandler(Some(writer)).withSavedOutput,
        docDir,
        props.Java().getAbsolutePath,
        scalaToolsClasspath,
        Nil,
        optsFile)

      cmd.exec() match {
        case 0 => TaskResult.success(this, sw)
        case _ => TaskResult.failure(this, sw, cmd.savedOutput)
      }
    }
    else {
      log.debug("not generating doc for module " + baseProject.toString)
      TaskResult.success(this, sw)
    }
  }
}
