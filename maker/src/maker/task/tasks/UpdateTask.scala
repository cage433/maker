package maker.task.tasks

import maker.project.Module
import maker.utils.FileUtils._
import maker.task._
import maker.utils.{Stopwatch, TableBuilder, Int}
import maker.{Resource, ResourceUpdater}
import maker.utils.RichString._
import scala.collection.JavaConversions._
import org.scalatest.Failed
import java.net.URL
import java.io.File

/**
  * Updates any missing resources. If any jars are missing then will try 
  * to download BOTH the binary and any associated source jar. 
  * If `forceSourceUpdate` is true then will try to download ALL missing source jars 
  *
  * Missing source jars are not treated as a cause for failure unless `forceSourceUpdate`
  * is true
  */
case class UpdateTask(module : Module, forceSourceUpdate : Boolean) extends Task
{
  def baseProject = module
  def name = "Update " + module

  def upstreamTasks : List[Task] = Nil

  private def removeRedundantResourceFiles(){
    def removeRedundant(resources : Seq[Resource], directory : File) {
      val expected = resources.map{resource => file(directory, resource.basename).asAbsoluteFile}.toSet
      val actual = directory.safeListFiles.map(_.asAbsoluteFile).toSet
      (actual -- expected).foreach(_.delete)
    }
    removeRedundant(module.resources(), module.managedLibDir)
    removeRedundant(module.resources(), module.managedResourceDir)
    removeRedundant(module.sourceJarResources(), module.managedLibSourceDir)
  }

  private def updateResources(resources : List[Resource]) = {
    resources.flatMap{
      resource => 
        new ResourceUpdater(resource, module.config, module.managedLibDir).update().errors
    }
  }

  def exec(results : Iterable[TaskResult], sw : Stopwatch) : TaskResult = {
    removeRedundantResourceFiles()
    val missingResources = module.resources().filterNot{resource => file(module.managedLibDir, resource.basename).exists}
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
