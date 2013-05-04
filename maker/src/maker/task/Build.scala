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
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedBlockingQueue
import maker.MakerProps
import maker.utils.MakerLog
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory
import maker.utils.FileUtils
import ch.qos.logback.classic.Level
import maker.task.compile.CompileTask
import maker.utils.RichString._

case class Build(
  originalTask : Task,
  graph : Dependency.Graph
) {

  require(graph.nodes.contains(originalTask), "Bad graph " + graph + ", doesn't contain " + originalTask)
  Build.clearTimings

  override def toString = {
    val buf = new StringBuffer
    buf.append("Build\n  OriginalTask: " + originalTask + "\n")
    buf.append("  Graph:\n  " + graph.toString.split("\n").mkString("\n  ") + "\n")
    buf.toString
  }
  

  val props = originalTask.props
  val log = props.log

  var roundNo = 0
  val monitor = new TaskMonitor(graph)

  val executor = {
    val nWorkers = props.NumberOfTaskThreads()
    val taskNumber = Build.taskCount.getAndIncrement
    val threadCount = new AtomicInteger(0)
    new ThreadPoolExecutor(
      nWorkers, 
      nWorkers, 
      Long.MaxValue, 
      TimeUnit.NANOSECONDS, 
      new LinkedBlockingQueue[Runnable](),
      new ThreadFactory(){
        def newThread(r : Runnable) = {
          val thread  = Executors.defaultThreadFactory.newThread(r)
          thread.setName(originalTask + "-" + taskNumber + "." + threadCount.getAndIncrement)
          thread
        }
      }
    )
  }

  class TaskMonitor(graph : Dependency.Graph){
    private val lock = new Object
    var results = List[TaskResult]()
    private var completed = Set[Task]()
    private var queued = Set[Task]()
    private var running = Set[Task]()
    private var unacknowledged = Set[Task]()
    private var remaining = graph
    private var taskLaunchCounter = 0

    def isComplete = lock.synchronized{
      completed.size == graph.size || results.exists{r ⇒ r.failed && r.task.failureHaltsTaskManager}
    }

    def isRunning = lock.synchronized{
      ! running.isEmpty
    }

    def addToQueue(t : Task){
      lock.synchronized{
        queued += t
        remaining -= t
      }
    }
    def addLaunch(t : Task){
      lock.synchronized{
        queued -= t
        running += t
      }
    }

    def addResult(t : Task, r : TaskResult){
      lock.synchronized{
        results ::= r
        running -= t
        completed += t
        unacknowledged += t
      }
    }

    def doLog{
      lock.synchronized{
        if (unacknowledged.nonEmpty){
          unacknowledged = Set[Task]()
          log.debug("Queue size " + executor.getQueue.size)
          log.debug("queued " + queued.size + queued.toList.mkString("\n\t", "\n\t", ""))
          log.debug("running " + running.size + running.toList.mkString("\n\t", "\n\t", ""))
          log.debug("all " + graph.size)
          log.debug("completed " + completed.size)
        }
      }
    }

    def nextAndResultsSoFar = lock.synchronized{
      val next = remaining.leaves.filter{
        pt ⇒ 
          graph.upstreams(pt).subsetOf(completed)
      }
      (next, Set[TaskResult]() ++ results)
    }
    def incCounter = lock.synchronized{
      taskLaunchCounter += 1
      taskLaunchCounter
    }
    def numRunning = lock.synchronized{
      running.size
    }
    def numQueued = lock.synchronized{
      executor.getQueue.size
    }
  }


  private def passToExecutor(pt : Task, resultsSoFar : Set[TaskResult]){
    val sw = new Stopwatch
    log.debug("Passing " + pt + " to executor")
    sw.snapshot(TaskResult.TASK_LAUNCHED)
    monitor.addToQueue(pt)
    executor.execute(
      new Runnable() {
        def run() {
          def threadNumber = Thread.currentThread.getName.last.toString.toInt
          try {
            Build.startTask(pt, threadNumber)
            val result = try {
              // Reassure the user some activity is happening
              if (Level.INFO.isGreaterOrEqual(props.MakerLogLevel())){
                if (monitor.incCounter % 20 == 0)
                  println
                print(".")
              }
              monitor.addLaunch(pt)
              log.debug("Launched " + pt + " (" + monitor.numRunning + " running, " + monitor.numQueued + " queued)")
              pt.exec(resultsSoFar.toList, sw)
            } catch {
              case e =>
                log.warn("exception thrown:" + e + " when running task " + pt)
                e.printStackTrace
                TaskResult.failure(pt, sw, e)
            }
            sw.snapshot(TaskResult.TASK_COMPLETE)
            monitor.addResult(pt, result.withRoundNo(roundNo))
            log.debug("Finished " + pt + " (" + monitor.numRunning + " running, " + monitor.numQueued + " queued)")
          } catch {
            case e ⇒ 
              log.warn("Exception " + e + " thrown during " + pt)
              e.printStackTrace
              System.exit(-1)
          }
          finally {
            monitor.synchronized{ monitor.notifyAll }
            Build.endTask(pt, threadNumber)
          }
        }
      }
    )
  }

  def execute : BuildResult = {

    val taskResults = log.infoWithTime("" + originalTask){
      originalTask.project.setUp(graph)
      execTasksInGraph
    }
    val buildResult = BuildResult(
      taskResults.toList,
      graph,
      originalTask
    )
    originalTask.project.tearDown(graph, buildResult)
    if (buildResult.failed && originalTask.props.ExecMode()){
      log.error(originalTask + " failed ")
      System.exit(-1)
    }
    BuildResult.lastResult.set(Some(buildResult))
    buildResult

  }

  private def execTasksInGraph = {

    while (! monitor.isComplete){
      monitor.doLog

      val (next, results) = monitor.nextAndResultsSoFar

      next.foreach {
        pt ⇒ 
          passToExecutor(pt, results)
      }

      if (next.nonEmpty)
        roundNo += 1

      monitor.synchronized{ monitor.wait(100) }
      monitor.doLog
    }
    waitForRunningTasksToComplete
    executor.shutdown
    val (_, results) = monitor.nextAndResultsSoFar

    results
  }

  private def waitForRunningTasksToComplete{
    while (monitor.isRunning){
      // We get here if a task failure causes early interruption
      // We still need to wait for other tasks to finish, if not
      // maker's state (e.g. file dependencies) coule be corrupted
      log.debug("Waiting for tasks to complete")
      Thread.sleep(100)
    }
  }

  def filter(predicate : Task ⇒ Boolean) = Build(originalTask, graph.filter(predicate))
}

object Build{
  val taskCount = new AtomicInteger(0)
  def apply(task : Task) : Build = Build(task, Dependency.Graph.transitiveClosure(task))
  def singleTask(task : Task) : Build = Build(task, new Dependency.Graph(Set(task)))
  var timings : List[(Task, Int, Boolean, Long)] = Nil
  def clearTimings{
    synchronized{
      timings = Nil
      compileTime = 0
      starts = Map.empty
      ends = Map.empty
    }
  }
  var starts : Map[Task, Long] = Map.empty
  var ends : Map[Task, Long] = Map.empty
  var compileTime : Long = 0

  def startTask(task : Task, threadNo : Int){
    synchronized{
      val time = System.currentTimeMillis
      starts += (task → time)
      timings ::= (task, threadNo, true, time)
    }
  }
  def endTask(task : Task, threadNo : Int){
    synchronized{
      val time = System.currentTimeMillis
      ends += (task → time)
      timings ::= (task, threadNo, false, time)
    }
  }
  def totalTaskTime = {
    val t = ends.values.toList.sum - starts.values.toList.sum
    t / 1000.0
  }
  def addCompileTime(t0 : Long, t1 : Long){
    synchronized{
      compileTime += (t1 - t0)
    }
  }

  def printTimings{
    val timings = ends.map{
      case (task, endTime) ⇒ 
        val startTime : Long = starts.getOrElse(task, 0)
        (endTime - startTime, task)
    }.toList
    timings.sortWith(_._1 < _._1).foreach{
      case (time, task) ⇒ 
        val t = (time / 1000.0).toString.padRight(8)
        val msPerFile = if (task.numberOfSourceFiles > 0) time / task.numberOfSourceFiles else 0
        println(t + msPerFile.toString.padRight(8) + task.numberOfSourceFiles.toString.padRight(8) + task.toShortString)
    }
  }

  def printFlow{
    var currentTasks : Map[Task, Int] = Map.empty
    val startTime : Long = timings.lastOption.map(_._4).getOrElse(0)
    var time = startTime
    timings.reverse.foreach{
      case (task, threadNo, started, milliTime) ⇒ {
        if (started){
          assert(! currentTasks.contains(task))
          assert(! currentTasks.values.toSet.contains(threadNo))
          currentTasks += (task → threadNo)
          printLine
        } else {
          assert(currentTasks.contains(task))
          assert(currentTasks.values.toSet.contains(threadNo))
          currentTasks -= task
        }
        def printLine{
          val reverseMap = currentTasks.map{ case (k, v) ⇒ (v, k)}.toMap
          val timeTaken = ((milliTime - startTime) / 1000.0).toString.padRight(6)
          val timeSinceLastTask = if (time == 0) milliTime / 1000.0 else (milliTime - time) / 1000.0
          println
          println("* " + timeSinceLastTask)
          time = milliTime
          print(timeTaken)
          (0 until MakerProps().NumberOfTaskThreads()).foreach {
            threadNo ⇒ 
              print(reverseMap.get(threadNo).map(_.toShortString).getOrElse("").padLeft(15) + ", ")
          }
          println
        }
      }
    }
  }
}

