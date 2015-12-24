package maker.task

import java.lang.Runnable
import java.util.Comparator
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger
import maker.project.Module
import maker.task.compile.CompileTask
import maker.task.tasks.{RunUnitTestsTask, UpdateTask}
import maker.utils.{Stopwatch, Int}
import ch.qos.logback.classic.Logger
import maker.Log

case class Build(
  name : String,
  graph : Dependency.Graph,
  maybeNumberOfWorkers : Option[Int]
) extends Log {

  override def toString = "Build " + name
  
  def tasks = graph.nodes

  def execute = new Execute().execute
  class Execute{

    val noWorkers = maybeNumberOfWorkers.getOrElse(
      (Runtime.getRuntime.availableProcessors / 2 max 1) min 4
    )
    val executor = Build.PriorityExecutor(noWorkers, name)

    val monitor = new Build.TaskMonitor(graph, executor)

    def execute : BuildResult = {
      val taskResults = execTasksInGraph()
      val buildResult = BuildResult(
        name,
        taskResults.toList,
        graph
      )
      buildResult
    }

    private def passToExecutor(pt : Task, resultsSoFar : Set[TaskResult]){
      val sw = new Stopwatch
      monitor.addToQueue(pt)
      val priority = 0
      logger.debug("SUBMITTING " + pt + " with priority " + priority)
      executor.executeWithPriority(priority){
        logger.debug("EXECUTING " + pt + " with priority " + priority)
            val threadNumber = Thread.currentThread.getName.last.toString.toInt
            try {
              val result = try {
                // Reassure the user some activity is happening
                if (monitor.incCounter % 20 == 0)
                  println
                print(".")
                monitor.addLaunch(pt)
                logger.info("Launched " + pt + " (" + monitor.numRunning + " running, " + monitor.numQueued + " queued)")
                val res = pt.exec(resultsSoFar.toList, sw)
                res
              } catch {
                case e: Throwable =>
                  logger.warn("exception thrown:" + e + " when running task " + pt)
                  e.printStackTrace
                  DefaultTaskResult(pt, false, sw, exception = Some(e))
              }
              sw.takeSnapshot(TaskResult.TASK_END)
              monitor.addResult(pt, result)
              logger.debug("Finished " + pt + " (" + monitor.numRunning + " running, " + monitor.numQueued + " queued)")
            } catch {
              case e: Throwable ⇒
                logger.warn("Exception " + e + " thrown during " + pt)
                e.printStackTrace
                System.exit(-1)
            }
            finally {
              monitor.synchronized{ monitor.notifyAll }
            }
          }
    }


    private def execTasksInGraph() = {

      while (! monitor.isComplete){
        monitor.doLog

        val (next, results) = monitor.nextAndResultsSoFar

        next.foreach {
          pt => 
            passToExecutor(pt, results)
        }

        monitor.synchronized{ monitor.wait(100) }
        monitor.doLog
      }
      waitForRunningTasksToComplete()
      executor.shutdown
      val (_, results) = monitor.nextAndResultsSoFar

      results
    }

    private def waitForRunningTasksToComplete(){
      while (monitor.isRunning){
        // We get here if a task failure causes early interruption
        // We still need to wait for other tasks to finish, if not
        // maker's state (e.g. file dependencies) coule be corrupted
        logger.debug("Waiting for tasks to complete")
        Thread.sleep(100)
      }
    }
  }
}

object Build{
  val taskCount = new AtomicInteger(0)

  private class PrioritisedFutureTask(r: Runnable, val priority: Int)
      extends FutureTask[Unit](r, {}) with Comparable[PrioritisedFutureTask] {
    override def compareTo(o: PrioritisedFutureTask) = priority - o.priority
  }

  /** `Executor` that can take a priority value (lower is higher
    * priority) along with a closure which is used when assigning work
    * to `Thread`s. Tasks without a priority value will be treated as
    * low priority.
    *
    * Note that once a task has been accepted, it will not be stopped, so
    * a low priority task that has been accepted will continue to run
    * even if high priority tasks are subsequently submitted.
    */
  class PriorityExecutor private(numberOfWorkers: Int, queue: BlockingQueue[Runnable])
      extends ThreadPoolExecutor(
        numberOfWorkers, 
        numberOfWorkers, 
        Long.MaxValue, 
        TimeUnit.NANOSECONDS, 
        queue
      ) {
    def executeWithPriority(priority: Int)(f: => Unit): Unit = {
      val task = new PrioritisedFutureTask(new Runnable {
        override def run(): Unit = f
      }, priority)
      super.execute(task)
    }
  }

  object PriorityExecutor {
    def apply(numberOfWorkers: Int, name: String): PriorityExecutor = {
      // should really be a PriorityExecutor[ComparableFutureTask] but
      // we are forced by invariance to use reflection and handle
      // Runnables
      val comparator = new Comparator[Runnable] {
        type P = PrioritisedFutureTask
        def compare(a: Runnable, b: Runnable) =
          if (a.isInstanceOf[P] && b.isInstanceOf[P])
            a.asInstanceOf[P].compareTo(b.asInstanceOf[P])
          else if (a.isInstanceOf[P]) -1
          else if (b.isInstanceOf[P]) 1
          else 0
      }
      val queue = new PriorityBlockingQueue[Runnable](32, comparator)
      val factory = new ThreadFactory(){
        val threadCount = new AtomicInteger(0)
        def newThread(r : Runnable) = {
          val thread  = Executors.defaultThreadFactory.newThread(r)
          thread.setName("Build-" + Build.taskCount.getAndIncrement + "-" + name + "." + threadCount.getAndIncrement)
          thread
        }
      }
      new PriorityExecutor(numberOfWorkers, queue)
    }
  }


  class TaskMonitor(graph : Dependency.Graph, executor : ThreadPoolExecutor) extends Log {
    private val lock = new Object
    var results = List[TaskResult]()
    private var completed = Set[Task]()
    private var queued = Set[Task]()
    private var running = Set[Task]()
    private var unacknowledged = Set[Task]()
    private var remaining = graph
    private var taskLaunchCounter = 0

    def isComplete = lock.synchronized{
      completed.size == graph.size || results.exists{r => r.failed && r.task.failureHaltsTaskManager}
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
          logger.debug("Queue size " + executor.getQueue.size)
          logger.debug("queued " + queued.size + queued.toList.mkString("\n\t", "\n\t", ""))
          logger.debug("running " + running.size + running.toList.mkString("\n\t", "\n\t", ""))
          logger.debug("all " + graph.size)
          logger.debug("completed " + completed.size)
        }
      }
    }

    def nextAndResultsSoFar = lock.synchronized{
      val next = remaining.leaves.filter{
        pt => 
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
}
