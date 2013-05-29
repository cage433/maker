package maker.task

import java.io.File
import maker.utils.RichString._
import maker.utils.Stopwatch
import maker.utils.Utils
import maker.task.compile.CompilationInfo
import maker.task.tasks.RunUnitTestsTask
import maker.utils.TaskInfo


trait TaskResult {
  def task : Task
  def sw : Stopwatch
  def succeeded : Boolean
  def failed = !succeeded
  def status = if (succeeded) "succeeded" else "failed"
  def roundNo : Option[Int]
  def timeTaken(name : String) = (timeAt(name) - sw.startTime) / 1000000
  def timeAt(name : String) = sw.snapshotTime(name).get
  def info : Option[TaskInfo]

  protected def copy_(
    task : Task = task, 
    sw : Stopwatch = sw,
    roundNo : Option[Int] = roundNo,
    info : Option[TaskInfo] = info
  ) : TaskResult
  def withRoundNo(no : Int) = copy_(roundNo = Some(no))
  def withInfo(info : TaskInfo) = copy_(info = Some(info))
  def withoutInfo = copy_(info = None)
  def withTask(newTask : Task) : TaskResult = copy_(task = newTask)
  def snapshot(name : String) = copy_(sw = sw.snapshot(name))
  def compilationInfo : Option[CompilationInfo] = info match {
    case Some(x) if x.isInstanceOf[CompilationInfo] ⇒ Some(x.asInstanceOf[CompilationInfo])
    case _ ⇒ None
  }

  override def toString = task + " " + status

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

case class TaskSucceeded(
  task : Task,
  sw : Stopwatch,
  roundNo : Option[Int] = None, 
  info : Option[TaskInfo] = None	  
) extends TaskResult {

  val succeeded = true

  protected def copy_(
    task : Task = task, 
    sw : Stopwatch = sw,
    roundNo : Option[Int] = roundNo,
    info : Option[TaskInfo] = info
  ) : TaskResult = copy(
    task = task, 
    sw = sw,
    roundNo = roundNo,
    info = info
  )

}

case class TaskFailed(
  task : Task,
  sw : Stopwatch,
  roundNo : Option[Int] = None,
  reasonForFailure : Option[String] = None,
  exception : Option[Throwable] = None,
  info : Option[TaskInfo] = None
) extends TaskResult {

  protected def copy_(
    task : Task = task, 
    sw : Stopwatch = sw,
    roundNo : Option[Int] = roundNo,
    info : Option[TaskInfo] = info
  ) : TaskResult = copy(
    task = task, 
    sw = sw,
    roundNo = roundNo,
    info = info
  )

  val succeeded = false

  override def toString = {
    val b = new StringBuffer
    b.append(super.toString + reasonForFailure.map(". " + _).getOrElse(""))
    
    (task, exception) match {
      case (_, Some(e)) ⇒ 
        b.addLine("Exception message")
        b.addLine(Option(e.getMessage).getOrElse("").indent(2))
        b.addLine("Stack trace")
        b.addLine(Utils.stackTraceAsString(e).indent(2))
      case (_ : RunUnitTestsTask, _) ⇒ info.foreach(b.append)
      case _ ⇒ 
    }
    b.toString
  }

  def filesWithChangedSigs : Set[File] = Set[File]()
}

object TaskResult{
  def success(task : Task, sw : Stopwatch) : TaskResult = TaskSucceeded(
    task,
    sw
  ).snapshot(EXEC_COMPLETE)

  def failure(task : Task, sw : Stopwatch, reason : String) : TaskResult = TaskFailed(
    task,
    sw,
    reasonForFailure = Some(reason)
  ).snapshot(EXEC_COMPLETE)

  def failure(task : Task, sw : Stopwatch, exception : Throwable) : TaskResult = TaskFailed(
    task,
    sw,
    reasonForFailure = Some(exception.getMessage),
    exception = Some(exception)
  ).snapshot(EXEC_COMPLETE)

  val EXEC_START = "Exec start"
  val EXEC_COMPLETE = "Exec complete"
  val TASK_COMPLETE = "Task complete"
  val TASK_LAUNCHED = "Task launched"
  val WORKER_RECEIVED = "Worked received"
  val WORKER_COMPLETE = "Worked complete"
}

