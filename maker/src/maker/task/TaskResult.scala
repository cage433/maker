package maker.task

import java.io.File
import maker.utils.RichString._
import maker.utils.Stopwatch
import maker.utils.Utils
import maker.task.compile.CompilationInfo
import maker.task.tasks.RunUnitTestsTask
import maker.utils.TaskInfo
import maker.utils.RichThrowable._

case class TaskResult(
  task : Task, 
  succeeded : Boolean, 
  info : Option[Any] = None, 
  message : Option[String] = None, 
  exception : Option[Throwable] = None
){
  def failed = !succeeded
  def status = if (succeeded) "succeeded" else "failed"

  def compilationInfo : Option[CompilationInfo] = info match {
    case Some(x) if x.isInstanceOf[CompilationInfo] ⇒ Some(x.asInstanceOf[CompilationInfo])
    case _ ⇒ None
  }

  override def toString = {
    if (succeeded)
      task + " " + status
    else {
      val b = new StringBuffer
      b.append(super.toString + message.map(". " + _).getOrElse(""))

      (task, exception) match {
        case (_, Some(e)) =>
          b.addLine("Exception message")
          b.addLine(Option(e.getMessage).getOrElse("").indent(2))
          b.addLine("Stack trace")
          b.addLine(e.stackTraceAsString.indent(2))
        case (_ : RunUnitTestsTask, _) => info.foreach(b.append)
        case _ =>
      }
      b.toString
    }
  }


  override def hashCode = task.hashCode
  override def equals(rhs : Any) = {
    rhs match {
      case p : TaskResult if task == p.task && (succeeded == p.succeeded) ⇒ {
        //I believe this assertion should always hold. It's really here so that
        //this overriden equals method never returns true on differing TaskResults
        assert(this eq p, "Shouldn't have two task results pointing to the same task")
        true
      }
      case _ ⇒ false
    }
  }
}

object TaskResult{
  def success(task : Task, info : Option[Any] = None) : TaskResult = TaskResult(
    task,
    succeeded = true,
    info = info
  )

  def failure(task : Task, info : Option[Any] = None, message : Option[String] = None, exception : Option[Throwable] = None) : TaskResult = TaskResult(
    task,
    succeeded = false,
    info = info,
    message = message,
    exception = exception
  )

}


