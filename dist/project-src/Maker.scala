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



package maker

import maker.project._
import maker.utils.FileUtils._
import java.io.File
import org.apache.commons.lang3.StringUtils
import maker.task.TaskResult._
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.Level._
import task.BuildResult
import maker.task.Task
import maker.utils.os.Command
import maker.utils.os.CommandOutputHandler
import maker.project.extras.TmuxMessaging
import scala.collection.mutable.{Map ⇒ MMap}
import maker.task.compile._

/**
 * this is Maker's own self-build definition,
 *   e.g. loaded after a maker.sh -b bootstrap
 */
object Maker {
  val MAKER_VERSION = ".1"

  private val makerOwnDefaultProps = MakerProps(
    MMap(
      // Publishing related parameters
      "DefaultPublishResolver" → "maker-oss-snapshot",
      // Pom generation related params
      "Organisation" → "maker",
      "GroupId" → "com.google.code.maker",
      "PomTemplateFile" → "pom.template",
      "PomBuildTemplateFile" → "pom-build.template",
      "ScmUrl" → "https://github.com/cage433/maker",
      "ScmConnection" → "git@github.com:cage433/maker.git",
      "Licenses" → "<license><name>GNU GENERAL PUBLIC LICENSE</name><url>http://www.gnu.org/licenses/gpl.html</url><distribution>repo</distribution></license>",
      "Developers" → "<developer><id>bob</id><name>bob></name><email>bob@bob.com</email></developer>"
    ) ++ MakerProps.propsFileToMap(file("Maker.conf"))
  )

  def mkProject(name : String, upstreamProjects : Project*) = {
    val root = file(name).asAbsoluteFile
    new Project(
      root,
      name,
      layout = new MakerProjectLayout(root),
      upstreamProjects = upstreamProjects.toList,
      props = makerOwnDefaultProps
    ) with TmuxMessaging with MoreSugar
  }

  val topLevelExcludedFolders : List[String] = Nil

  lazy val testReporterProj = mkProject("test-reporter")
  lazy val utilsProj = mkProject("utils", testReporterProj)
  lazy val makerProj = mkProject("maker", utilsProj)

  lazy val makerAll = new TopLevelProject("makerAll", List(testReporterProj, utilsProj, makerProj))

  lazy val mkr = makerProj

  var verboseTestOutput : Boolean = true
  var verboseTaskOutput : Boolean = true

  def analyseBuild(r : BuildResult){
    val execTimes = r.results.map(_.timeTaken(EXEC_COMPLETE))
    val taskTimes = r.results.map(_.timeTaken(TASK_COMPLETE))

    println("Total exec time " + execTimes.sum)
    println("Total task time " + taskTimes.sum)
  }

  // Used to disambiguate which maker is running in the repl.
  def pwd = println(System.getProperty("user.dir"))
  def debug = makerProj.log.setLevel(DEBUG)
  def info = makerProj.log.setLevel(INFO)

  trait MoreSugar{
    self : Project ⇒ 
      def tcc = testCompileContinuously
      def stfe {
        props.ShowFailingTestException := ! props.ShowFailingTestException()
      }
  }

}
