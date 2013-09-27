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
import maker.task._
import maker.utils.Stopwatch


/** Clean task - cleans up all build artifacts from the classpath
  *
  *  removes all build content and directories that contained it
  */
case class CleanTask(module : Module, deleteManagedLibs : Boolean = false) extends Task {
  def name = "Clean"
  def upstreamTasks = (module.immediateUpstreamModules ::: module.immediateUpstreamTestModules).distinct.map(CleanTask(_, deleteManagedLibs))

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    val props = module.props
    val log = props.log
    log.debug("cleaning " + module)
    if (deleteManagedLibs){
      Option(module.managedLibDir.listFiles).foreach(_.foreach(_.delete))
    }

    // remove all output as we don't want lingering files or even empty dirs messing up a subsequent builds
    module.outputArtifact.delete

    cleanRegularFilesLeavingDirectories(module.compilePhase.outputDir)
    cleanRegularFilesLeavingDirectories(module.testCompilePhase.outputDir)
    cleanRegularFilesLeavingDirectories(module.packageDir)
    cleanRegularFilesLeavingDirectories(module.docOutputDir)
    cleanRegularFilesLeavingDirectories(module.targetDir)
    recursiveDelete(module.compilePhase.phaseDirectory)
    recursiveDelete(module.testCompilePhase.phaseDirectory)
    module.compilePhase.compilationCacheFile.delete
    module.testCompilePhase.compilationCacheFile.delete

    TaskResult.success(this, sw)
  }
}
