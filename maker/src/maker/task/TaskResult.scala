package maker.task

import java.io.File
import maker.utils.RichString._
import maker.utils.Stopwatch
import maker.task.tasks.RunUnitTestsTask
import maker.utils.RichThrowable._
import maker.task.tasks.UpdateTask
import scala.collection.JavaConversions._
import maker.utils.TableBuilder
import maker.task.compile.CompileTask
import maker.utils.Timings

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
  def intervalStartAndEndTime : Map[String, (Long, Long)] = {
    intervalNames.flatMap{
      name => 
        stopwatch.intervalStartAndEndTime(name).map{
          case (t1, t2) => (name -> (t1, t2))
        }
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

  private def NANOS_PER_SECOND = 1000000000
  def fmtNanos(timeInNanos : Long) = "%.1f".format(timeInNanos * 1.0 / NANOS_PER_SECOND)
  def COLUMN_WIDTHS = List(30, 25, 15, 15)

  def clockAndCPUTime(tr : List[TaskResult]) : (Long, Long) = {
    val taskTimes = tr.map{case tr => (tr.startTime, tr.endTime)}
    Timings(taskTimes).clockAndCPUTime
  }

  def reportOnTaskTimings(taskResults : List[TaskResult]){
    val MIN_TIME = 0.05
    val tb = TableBuilder(
      "Task".padRight(COLUMN_WIDTHS(0)), 
      "Interval".padRight(COLUMN_WIDTHS(1)), 
      "CPU Time".padRight(COLUMN_WIDTHS(2)),
      f"Clock Time (if > $MIN_TIME%1.2fs)") 



    val resultsByName = taskResults.groupBy{
      case taskResult => taskResult.task.name
    }
    resultsByName.foreach{
      case (taskName, resultsForType) =>
        val (clockTime, cpuTime) = clockAndCPUTime(resultsForType)
        if (clockTime > NANOS_PER_SECOND * MIN_TIME)
          tb.addRow(taskName, "", fmtNanos(cpuTime), fmtNanos(clockTime))

        val intervalMaps : List[Map[String, (Long, Long)]] = resultsForType.map(_.intervalStartAndEndTime)
        val intervalNames : List[String] = intervalMaps.flatMap(_.keys.toList).distinct 

        intervalNames.foreach{
          name => 
            val intervalStartAndEndTimes : List[(Long, Long)] = intervalMaps.map(_.get(name)).collect{
              case Some((t1, t2)) => (t1, t2)
            }
            val (intervalClockTime, intervalCPUTime) = Timings(intervalStartAndEndTimes).clockAndCPUTime
            if (intervalClockTime > NANOS_PER_SECOND * MIN_TIME)
              tb.addRow(taskName, name, fmtNanos(intervalCPUTime), fmtNanos(intervalClockTime))
        }
    }
    val (totalClockTime, totalCpuTime) = clockAndCPUTime(taskResults)
    tb.addRow("All Tasks", "", fmtNanos(totalCpuTime), fmtNanos(totalClockTime))
    println(tb)
  }

  def reportOnFirstFailingTask(taskResults : List[TaskResult]){
    val failing = taskResults.find(_.failed).get
    println(("First failure was " + failing.task + "\n" + failing.message).inRed)
  }

} 

