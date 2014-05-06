package maker.task

import java.io.File
import maker.utils.RichString._
import maker.utils.Stopwatch
import maker.utils.Utils
import maker.task.compile.CompilationInfo
import maker.task.tasks.RunUnitTestsTask
import maker.utils.TaskInfo
import maker.utils.RichThrowable._
import maker.task.tasks.UpdateTask
import scala.collection.JavaConversions._
import maker.utils.TableBuilder
import maker.task.compile.CompileTask

trait TaskResult{
  def task : Task
  def succeeded : Boolean
  def stopwatch : Stopwatch
  private def info : Option[Any] = None
  def message : Option[String] 
  def exception : Option[Throwable] 
  def failed = !succeeded
  def status = if (succeeded) "succeeded" else "failed"

  def startTime = stopwatch.startTime
  def endTime = stopwatch.snapshotTime(TaskResult.TASK_END).getOrElse(startTime)
  def time = endTime - startTime

  def isTestResult = task match {
    case _ : RunUnitTestsTask => true
    case _ => false
  }
  def isCompileResult = task match {
    case _ : CompileTask => true
    case _ => false
  }
  def intervalNames : List[String] = stopwatch.snapshots.collect{
    case ((name : String, _), _) => name
  }.toList

  def intervalTimings : Map[String, Long] = {
    intervalNames.map{
      name => 
        val time : Long = stopwatch.intervalTime(name).getOrElse(-1)
        name -> time
    }.toMap
  }

  override def toString = {
    if (succeeded)
      task + " " + status
    else {
      val b = new StringBuffer
      b.append(task.toString + message.map(". " + _).getOrElse(""))

      (task, exception) match {
        case (_, Some(e)) =>
          b.addLine("Exception message")
          b.addLine(Option(e.getMessage).getOrElse("").indent(2))
          b.addLine("Stack trace")
          b.addLine(e.stackTraceAsString.indent(2))
        case _ =>
      }
      b.toString
    }
  }


  override def hashCode = task.hashCode
  override def equals(rhs : Any) = {
    rhs match {
      case p : TaskResult if task == p.task && (succeeded == p.succeeded) => {
        //I believe this assertion should always hold. It's really here so that
        //this overriden equals method never returns true on differing TaskResults
        assert(this eq p, "Shouldn't have two task results pointing to the same task")
        true
      }
      case _ => false
    }
  }
}

case class DefaultTaskResult(
  task : Task, 
  succeeded : Boolean, 
  stopwatch : Stopwatch,
  val info : Option[Any] = None, 
  override val message : Option[String] = None, 
  override val exception : Option[Throwable] = None
) extends TaskResult

object TaskResult{
  def TASK_END = "TASK_END"


} 

