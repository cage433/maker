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

package maker.task

import maker.utils.Stopwatch
import maker.project.BaseProject
import maker.project.Module
import maker.utils.TableBuilder
import maker.utils.RichString._

trait Task {
  def name : String
  def toShortString = toString
  def exec(results : Iterable[TaskResult] = Nil, sw : Stopwatch) : TaskResult
  def failureHaltsTaskManager : Boolean = true
  def baseProject : BaseProject

  override def toString = baseProject + " - " + name
  /**
   * Tasks that normally need to run BEFORE this one does 
   */
  def upstreamTasks : Iterable[Task] 
}

abstract class SingleModuleTask(module : Module)
  extends Task
{
  def baseProject = module
}

object Task {
  val termSym = "ctrl-]"
  val termChar = 29 // ctrl-]

  private val NANOS_PER_SECOND = 1000000000
  def fmtNanos(timeInNanos : Long) = "%.1f".format(timeInNanos * 1.0 / NANOS_PER_SECOND)
  val COLUMN_WIDTHS = List(30, 25, 15, 15)

  def reportOnTaskTimings(taskResults : List[TaskResult]){
    val tb = TableBuilder(
      "Task".padRight(COLUMN_WIDTHS(0)), 
      "Interval".padRight(COLUMN_WIDTHS(1)), 
      "CPU Time".padRight(COLUMN_WIDTHS(2)),
      "Clock Time") 
    val totalClockTime = taskResults.map(_.endTime).max - taskResults.map(_.startTime).min
    val totalCpuTime = taskResults.map(_.time).sum

    tb.addRow("All", "", fmtNanos(totalCpuTime), fmtNanos(totalClockTime))

    val resultsByType = taskResults.groupBy{
      case taskResult => taskResult.task.getClass.getSimpleName
    }
    val timesByType : List[(String, Long, Map[String, Long])] = resultsByType.map{
      case (typeName, resultsForType) =>
        val totalTime = resultsForType.map(_.time).sum
        val intervalMaps : List[Map[String, Long]] = resultsForType.map(_.intervalTimings)
        val intervalNames : List[String] = intervalMaps.flatMap(_.keys.toList).distinct 

        val netIntervals = intervalNames.map{
          name => 
            val time = intervalMaps.map(_.getOrElse(name, 0l)).sum
            name -> time
        }.toMap
        (typeName, totalTime, netIntervals)
    }.toList
    timesByType.sortWith(_._2 > _._2).foreach{
      case (typeName, totalTime, netIntervals) =>
        tb.addRow(typeName, "Total", fmtNanos(totalTime), "")
        if (netIntervals.nonEmpty){
          netIntervals.toList.sortWith(_._2 > _._2).foreach{
            case (intervalName, time) =>
              tb.addRow(typeName, intervalName, fmtNanos(time), "")
          }
        }
    }
    println(tb)
  }

  def reportOnFirstFailingTask(taskResults : List[TaskResult]){
    val failing = taskResults.find(_.failed).get
    println(("First failure was " + failing.task + "\n").inRed)
  }
}
