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
import maker.task.Task
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext
import maker.task._
import maker.utils.Stopwatch
import maker.project.WebAppDetails
import maker.MakerProps
import maker.project.WebProject

/**
 * run a simple web app using Jetty as a container
 */
case class RunJettyTask(project : Project with WebProject) extends Task {
  def name = "Run Jetty"
  def upstreamTasks = PackageWarTask(project) :: Nil

  def exec(results : List[TaskResult], sw : Stopwatch) : TaskResult = {
    val log = project.props.log

    project.webAppDetails match {
      case WebAppDetails(webAppDir, httpPort) => {
        log.info("running webapp of project " + project.name)

        //val httpPort = parameters.getOrElse("portNo", "8080").toInt
        val warFile = project.outputArtifact
        val server = new Server(httpPort)

        val contextPath = "/" + project.name
        val webAppCtx = new WebAppContext(warFile.getAbsolutePath, contextPath)
        webAppCtx.setServer(server)

        // run a standard java-ee classloader strategy, maker env provides suitable container classpath for servlets etc
        webAppCtx.setParentLoaderPriority(false)
        server.setHandler(webAppCtx)

        log.info("Starting HTTP on port: " + httpPort)
        server.start()

        log.info("Press ctrl-] to end...")

        def wait() {
          while (server.isRunning) {
            Thread.sleep(1000)
            if (System.in.available  > 0 && System.in.read == 29 /* ctrl-] */) {
              server.stop()
              return
            }
          }
        }
        wait()
        TaskResult.success(this, sw)
      }
    }
  }
}
