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

import org.apache.commons.io.FileUtils._
import maker.project._
import maker.task._
import maker.utils.Stopwatch
import maker.utils.maven.IvyLock
import maker.utils.FileUtils._
import maker.project.Project

/**
 * publishes poms and packaged artifacts to the local filesystem at ~/.ivy2/maker-local - subject to change
 */
case class CreateDeployTask(baseProject : BaseProject) extends Task {
  def name = "Create Deploy"
  val log = baseProject.log

  def upstreamTasks = baseProject.allUpstreamModules.map(PackageMainJarTask) ::: baseProject.allUpstreamModules.map(PackageTestJarTask)

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    println("Running CreateDeployTask")
    IvyLock.synchronized{
      doPublish(baseProject, results, sw)
    }
  }
  
  private def doPublish(baseProject: BaseProject, results : Iterable[TaskResult], sw : Stopwatch) = {

    baseProject match {
      case p : Project =>
        val baseOutputDir = file(p.rootAbsoluteFile,"/target-maker/deploy/")
        log.info("Creating deployment directory: " + baseOutputDir)
        baseOutputDir.deleteAll()
        baseOutputDir.mkdirs()

        val jarsDir = file(baseOutputDir,"/jars/")
        log.info("Creating jars directory: " + jarsDir)
        jarsDir.mkdirs()

        val thirdPartyJars = file(baseOutputDir,"/thirdpartyjars/")
        log.info("Creating thirdparty jars directory: " + thirdPartyJars)
        thirdPartyJars.mkdirs()

        val binDir = file(baseOutputDir,"/bin/")
        binDir.mkdirs()

        log.info("copying jars to jars directory")
        p.allUpstreamModules foreach { m =>
          copyFileToDirectory(m.outputArtifact, jarsDir)
        }

        log.info("Copying properties files")
        val propsDir = file(baseOutputDir,"/props/envs")
        propsDir.mkdirs()
        copyDirectory(file(p.rootAbsoluteFile,"/props/envs/"),propsDir)

        log.info("Copying scripts")
        copyDirectory(file(p.rootAbsoluteFile,"/bin/"),binDir)

        for ( m <- p.allModules ;
              j <- m.classpathJars)
          copyFile(j,file(thirdPartyJars,"/" + j.getName))

        val appJars = jarsDir.listFiles().toList.map(_.relativeTo(baseOutputDir)).map(_.getPath).sortWith(_<_)
        val tpJars = thirdPartyJars.listFiles().toList.map(_.relativeTo(baseOutputDir)).map(_.getPath).sortWith(_<_)

        val classpathString = (appJars ::: tpJars).mkString(":")
        writeToFile(file(baseOutputDir,"/bin/deploy-classpath.sh"), "export CLASSPATH=" + classpathString)

        // test files

        val testJarsDir = file(baseOutputDir,"/testjars/")
        log.info("Creating jars directory: " + testJarsDir)
        testJarsDir.mkdirs()

        log.info("copying test jars to testjars directory")
        p.allUpstreamModules foreach { m =>
          copyFileToDirectory(m.testOutputArtifact, testJarsDir)
        }

        val testJars = testJarsDir.listFiles().toList.map(_.relativeTo(baseOutputDir)).map(_.getPath).sortWith(_<_)
        val testClasspathString= (appJars ::: tpJars ::: testJars).mkString(":")
        writeToFile(file(baseOutputDir,"/bin/deploy-test-classpath.sh"), "export CLASSPATH=" + testClasspathString)

        copyDirectory(file(p.rootAbsoluteFile,"/reports.impl/resource_managed/"),file(baseOutputDir,"/reports.impl/resource_managed/"))
        copyDirectory(file(p.rootAbsoluteFile,"/databases/resource_managed/"),file(baseOutputDir,"/databases/resource_managed/"))
        copyDirectory(file(p.rootAbsoluteFile,"/launcher/resource_managed/"),file(baseOutputDir,"/launcher/resource_managed/"))
        copyDirectory(file(p.rootAbsoluteFile,"services/cs/TrinityService/files/"),file(baseOutputDir,"services/cs/TrinityService/files/"))




        writeToFile(file(baseOutputDir,"/git.txt"), "USE BINARY DEPLOY")

        log.info("complete")
      case m : Module =>
        throw new IllegalStateException("Should not be run on a module")
    }
    TaskResult.success(this, sw)
  }
}
