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
import maker.Resource
import maker.utils.TableBuilder
import maker.utils.RichString._
import scala.collection.JavaConversions._

/**
  * Updates any missing resources. If any jars are missing then will try 
  * to download BOTH the binary and any associated source jar. 
  * If `forceSourceUpdate` is true then will try to download ALL missing source jars 
  *
  * Missing source jars are not treated as a cause for failure unless `forceSourceUpdate`
  * is true
  */
case class UpdateTask(module : Module, forceSourceUpdate : Boolean) 
  extends SingleModuleTask(module)
{
  def name = "Update " + module

  def upstreamTasks : List[Task] = Nil

  private def removeRedundantResourceFiles(){
    module.resources().map(_.resourceFile).groupBy(_.dirname).foreach{
      case (dir, expectedResourceFiles) => 
        val actualResourceFiles = dir.safeListFiles.map(_.asAbsoluteFile).toSet
        (actualResourceFiles -- expectedResourceFiles.map(_.asAbsoluteFile)).foreach(_.delete)
    }
  }

  private def updateResources(resources : List[Resource]) = {
    resources.flatMap(_.update(module.props).errors)
  }

  def exec(results : Iterable[TaskResult], sw : Stopwatch) : TaskResult = {
    removeRedundantResourceFiles()
    val missingResources = module.resources().filterNot(_.resourceFile.exists)
    var errors : List[(Int, String)] = updateResources(missingResources)

    if (forceSourceUpdate){
      errors :::= updateResources(module.sourceJarResources())
    } else {
      val sourceJarsForMissing = module.sourceJarResources().filter{
        sourceResource => missingResources.exists{
          resource => 
            resource.groupId == sourceResource.groupId &&
            resource.artifactId == sourceResource.artifactId &&
            resource.version == sourceResource.version
        }
      }
      updateResources(sourceJarsForMissing)
    }


    if (errors.isEmpty)
      UpdateTaskResult(this, true, sw, Nil)
    else
      UpdateTaskResult(
        this, false, sw, errors,
        message = Some("Failed to update resource(s) ")
      )
  }
}

object UpdateTask{
  def reportOnUpdateFailures(taskResults : List[TaskResult]){
    val failures : List[(Int, String)] = taskResults.collect{
      case u : UpdateTaskResult => u.failures
    }.flatten
    if (failures.nonEmpty){
      val b = new StringBuffer
      val tb = TableBuilder("Curl Error Code   ", "URL")
      failures.foreach{
        case (returnCode, command) => 
          tb.addRow(returnCode.toString, command)
      }
      b.addLine("\n" + tb.toString)
      b.addLine("\n\n" + "Proxy settings may be the cause - env vars are ".inRed)
      val etb = TableBuilder("Variable              ", "Value")
      System.getenv().filterKeys(_.toLowerCase.contains("proxy")).foreach{
        case (variable, value) => 
          etb.addRow(variable, value.truncate(100))
      }
      b.addLine(etb.toString)
      println(b)
    }
  }
}

case class UpdateTaskResult(
  task : UpdateTask, 
  succeeded : Boolean, 
  stopwatch : Stopwatch,
  failures : List[(Int, String)],
  override val message : Option[String] = None, 
  override val exception : Option[Throwable] = None
) extends TaskResult
