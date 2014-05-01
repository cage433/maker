package maker.task

import java.io.File
import maker.task.TaskResult._
import maker.utils.Stopwatch
import maker.utils.RichString._
import maker.MakerProps
import java.util.concurrent.atomic.AtomicReference
import maker.utils.RichIterable._
import maker.utils.MakerTestResults
import maker.utils.TestIdentifier


case class BuildResult(
  name : String,
  results : List[TaskResult],
  graph : Dependency.Graph
) {

  self =>

  def succeeded = results.forall(_.succeeded)
  def failed = !succeeded

  def maybeFirstFailure : Option[TaskResult] = results.reverse.find(_.failed)
  def toString_ = {
    assert(results.size > 0, "Expected some tasks to execute")

    val buffer = new StringBuffer

    def reportNumberOfScalaFilesCompiled{
      println("Debug: BuildResult: TODO reportNumberOfScalaFilesCompiled")
    }

    if (succeeded){
      buffer.append(name + " succeeded")
      buffer.toString inBlue
    } else {
      val firstFailure : TaskResult = maybeFirstFailure.get
      buffer.append(name + " failed. First failing upstream task is\n\n")
      buffer.append(firstFailure + "")
      reportNumberOfScalaFilesCompiled
      buffer.toString inRed
    }
  }

  private val NANOS_PER_SECOND = 1000000000
  def reportTopLevelTiming{
    if (failed || results.isEmpty){
      return
    }
    val clockTime = results.map(_.endTime).max - results.map(_.startTime).min
    val cpuTime = results.map(_.time).sum
    println
    println("Clock time  = %.1f (s)".format(clockTime * 1.0 / NANOS_PER_SECOND))
    println("CPU time    = %.1f (s)" .format(cpuTime * 1.0 / NANOS_PER_SECOND))
    println("Parallelism = %.1f".format((cpuTime  * 1.0 / clockTime)))
  }

  def reportTimingsByTaskType{
    val resultsByType = results.groupBy{
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
        println(typeName + " took " + (totalTime / NANOS_PER_SECOND) + " (s)")
        if (netIntervals.nonEmpty){
          netIntervals.toList.sortWith(_._2 > _._2).foreach{
            case (intervalName, time) =>
              println("\t" + intervalName + " took " + (time / NANOS_PER_SECOND) + " (s)")
          }
        }
    }
  }
  def testResults = {
    val testResultsList : List[MakerTestResults] = results.collect{
      case TaskResult(_, _, _, Some(mtr : MakerTestResults), _, _) => mtr
    }.toList
    val combinedTestResults = testResultsList.fold(MakerTestResults())(_++_)
    combinedTestResults
  }

  def reportSlowTestSuites{
    println("\nSlowest 5 test suites")
    println("Suite".padRight(30) + "Suite Time".padRight(10) + "\t" + "Test Time".padRight(10) + "\tNum Tests")
    testResults.orderedSuiteTimes.take(5).foreach{
      case (suite, clockTime, cpuTime, numTests) =>
      println(
        suite.toString.padRight(30) + clockTime.toString.padRight(10) + "\t" + cpuTime.toString.padRight(10) + "\t" + numTests
      )
    }
  }

  def reportSlowUnitTests{
    println("\nSlowest 5 tests")
    println("Suite".padRight(30) + "Test".padRight(30) + "\t" + "Clock Time")
    testResults.testsOrderedByTime.take(5).foreach{
      case (TestIdentifier(suite, _, test), clockTime) => 
        println(
          suite.padRight(30) + test.take(30).padRight(30) + "\t" + (clockTime / NANOS_PER_SECOND)
        )
    }
  }

  override def toString = {
    // Nasty hack to get colourization to work in the repl
    println(toString_)
    ""
  }

  def withResult(taskResult : List[TaskResult]) =
    this.copy(results = taskResult ::: this.results)

  @inline
  final def flatMap(f : BuildResult => BuildResult) : BuildResult =
    if (succeeded) f(this).withResult(this.results) else this

  @inline
  final def map(f: BuildResult => BuildResult): BuildResult =
    if (succeeded) f(this) else this

  def filter(f: AnyRef => Boolean): BuildResult = this

  class WithFilter(p: AnyRef => Boolean) {
    def map[B](f: AnyRef => BuildResult): BuildResult = self.filter(p).map(f)
    def flatMap[B](f: AnyRef => BuildResult): BuildResult = self.filter(p).flatMap(f)
    def withFilter(q: AnyRef => Boolean): WithFilter =
      new WithFilter(x => p(x) && q(x))
  }

  // is called with conditional statement in for comprehension
  def withFilter(p: AnyRef => Boolean): WithFilter = new WithFilter(p)

  def failures = results.filter(_.failed)

  def compilationInfos = results.flatMap(_.compilationInfo)

}

case class LastResult(result : BuildResult){
  val succeeded = result.succeeded

  def newLine(implicit b : StringBuffer){b.append("\n")}
  def addLine(line : AnyRef)(implicit b : StringBuffer){b.append(line.toString); newLine}
  
  override def toString = {
    implicit val b = new StringBuffer
    //def newLine{b.append("\n")}
    newLine
    addLine("The last task run was " + result.name + ". It " + (if (succeeded) "succeeded" else "failed"))
    newLine
    addLine("Methods")
    addLine("list       - show a list of each result")
    addLine("apply(i)   - shows result 'i'")
    addLine("result(i)  - returns result 'i'")
    addLine("info(i)    - shows info for result 'i'")
    addLine("info       - shows info on the first failing task")
    b.toString
  }

  def list {
    implicit val b = new StringBuffer
    result.results.zipWithIndex.foreach{
      case (taskResult, i) => 
        addLine("  " + i + ": " + taskResult.status + " " + taskResult.task)
    }
    println(b.toString)
  } 

  def apply(i : Int){
    implicit val b = new StringBuffer
    addLine(result(i).toString)
    println(b.toString)
  }

  def result(i : Int) : TaskResult = result.results(i)
  def info(i : Int){
    result(i).info match {
      case Some(info) => println(info)
      case None => println("No info available")
    }
  }
  def info{
    result.maybeFirstFailure.flatMap(_.info) match {
      case Some(info) => println(info)
      case None => println("No info available")
    }
  }

}

object BuildResult{
  val lastResult : AtomicReference[Option[BuildResult]] = new AtomicReference(None)
  def last = LastResult(lastResult.get.get)
  private var compiledFiles = Map[File, Int]()
  def recordCompiledFiles(files : Set[File]){
    synchronized{
      files.foreach{
        f => 
          compiledFiles += (f -> (compiledFiles.getOrElse(f, 0) + 1))
      }
    }
  }
  def clear(){
    compiledFiles = Map[File, Int]()
    timings = Map[Set[String], Long]()
  }

  def printCompiledFiles{
    val b = new StringBuffer
    b.addLine("Compiled File Count")
    val byCount = compiledFiles.keySet.groupBy(compiledFiles(_))
    val counts = byCount.keySet.toList.sortWith(_>_).foreach{
      c => 
        b.addLine("  " + c)
        b.addLine(byCount(c).map(_.getName).toList.sortWith(_<_).asTable(3).indent("    "))
    }
    println(b.toString)
  }

  private var timings = Map[Set[String], Long]()
  def timingKeys = timings.keySet.flatten
  def totalTiming(key : String) = breakdown(key).values.sum
  def breakdown(key : String) = timings.filterKeys(_.contains(key))
  def addTiming(time : Long, keys : String*){
    val keysAsSet = keys.toSet
    synchronized{
      val newTime : Long = timings.getOrElse(keysAsSet, 0L) + time
      timings += (keysAsSet -> newTime)
    }
  }
  def printTimings{
   timingKeys.toList.map{
     k => (k, totalTiming(k))
   }.sortWith(_._2 < _._2).foreach{
      case (k, t) => 
        val indent = (50 - k.size) max 2
        println(k + (" " * indent) + t)
    }
  }

}

