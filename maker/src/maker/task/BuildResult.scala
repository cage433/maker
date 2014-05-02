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
import maker.utils.TableBuilder
import maker.utils.TestFailure
import maker.task.compile.CompilationInfo
import maker.task.compile.CompileTask
import maker.task.compile.CompilationFailedInfo
import sbt.compiler.CompileFailed
import xsbti.Position
import maker.utils.FileUtils._
import xsbti.Severity
import maker.project.Module
import xsbti.Problem


case class BuildResult(
  name : String,
  results : List[TaskResult],
  graph : Dependency.Graph
) {

  self =>

  def succeeded = results.forall(_.succeeded)
  def failed = !succeeded

  def maybeFirstFailure : Option[TaskResult] = results.reverse.find(_.failed)

  def reportResult{
    new ReportBuildResult(this).report()
  }

  def testResults = {
    val testResultsList : List[MakerTestResults] = results.collect{
      case TaskResult(_, _, _, Some(mtr : MakerTestResults), _, _) => mtr
    }.toList
    val combinedTestResults = testResultsList.fold(MakerTestResults())(_++_)
    combinedTestResults
  }


  override def toString = {
    name + (if (succeeded) " succeeded " else " failed")
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

case class ReportBuildResult(br : BuildResult){
  val results = br.results
  private val NANOS_PER_SECOND = 1000000000
  private val COLUMN_WIDTHS = List(30, 25, 15, 15)

  def fmt(timeInNanos : Long) = "%.1f".format(timeInNanos * 1.0 / NANOS_PER_SECOND)

  def timingsTable : TableBuilder = {
    val tb = TableBuilder(
      "Task".padRight(COLUMN_WIDTHS(0)), 
      "Interval".padRight(COLUMN_WIDTHS(1)), 
      "CPU Time".padRight(COLUMN_WIDTHS(2)),
      "Clock Time") 
    val totalClockTime = results.map(_.endTime).max - results.map(_.startTime).min
    val totalCpuTime = results.map(_.time).sum

    tb.addRow("All", "", fmt(totalCpuTime), fmt(totalClockTime))

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
        tb.addRow(typeName, "Total", fmt(totalTime), "")
        if (netIntervals.nonEmpty){
          netIntervals.toList.sortWith(_._2 > _._2).foreach{
            case (intervalName, time) =>
              tb.addRow(typeName, intervalName, fmt(time), "")
          }
        }
    }
    tb
  }

  def reportSlowTestSuites{
    println("\nSlowest 5 test suites".inBlue)
    val tb = TableBuilder(
      "Suite".padRight(COLUMN_WIDTHS(0)), 
      "Num Tests".padRight(COLUMN_WIDTHS(1)),
      "CPU Time".padRight(COLUMN_WIDTHS(2)),
      "Clock Time"
    )
    br.testResults.orderedSuiteTimes.take(5).foreach{
      case (suite, clockTime, cpuTime, numTests) =>
        tb.addRow(
          suite,
          numTests,
          fmt(cpuTime),
          fmt(clockTime)
        )
    }
    println(tb.toString)
  }

  def reportSlowUnitTests{
    println("\nSlowest 5 tests".inBlue)
    val tb = TableBuilder(
      "Suite".padRight(COLUMN_WIDTHS(0)), 
      "Test".padRight(COLUMN_WIDTHS(1) + COLUMN_WIDTHS(2)), 
      "Clock Time")
    br.testResults.testsOrderedByTime.take(5).foreach{
      case (TestIdentifier(suite, _, test), clockTime) => 
        tb.addRow(
          suite,
          test,
          fmt(clockTime)
        )
    }
    println(tb.toString)
  }

  def reportCompilationFailures{
    def position(prob : Problem) = {
      val p = prob.position
      var text = ""
      if (! p.sourceFile.isDefined)
        ("Unknown", "Unknown")
      else if (! p.line.isDefined)
        (p.sourceFile.get.basename, "Unknown")
      else
        (p.sourceFile.get.basename, p.line.get.toString)
    }

    val failures : List[(Module, String, String, String)] = br.results.collect{
      case TaskResult(
        task : CompileTask,
        false,
        _,
        Some(CompilationFailedInfo(compFailed)),
        _,
        _
      ) =>
        val severeProblems = compFailed.problems.filter(_.severity == Severity.Error)
        severeProblems.map{
          prob =>
            val (sourceFile, lineNo) = position(prob)
            (task.baseProject, prob.message, sourceFile, lineNo)
        }
    }.toList.flatten

    if (failures.nonEmpty){
      val tb = TableBuilder(
        "Module       ",
        "Message                              ", 
        "Line  ",
        "File                    ")
      failures.foreach{
        case (module, message, sourceFile, lineNo) => 
            tb.addRow(module, message, lineNo, sourceFile)
      }
      println(tb)
    }
  }

  def report(){
    if (br.failed){
      val bf = new StringBuffer
      val firstFailure : TaskResult = br.maybeFirstFailure.get
      bf.append(("Build failed. First failing upstream task is '").inRed)
      bf.addLine((firstFailure.task + "'\n").inRed)

      println(bf.toString)

      if (br.testResults.failures.nonEmpty){
        val tb = TableBuilder(
          "No  ",
          "Suite                   ",
          "Test                              ",
          "Message"
        )
        br.testResults.failures.zipWithIndex.foreach{
          case ((TestIdentifier(suite, _, test), TestFailure(message, _)), i) => 
            tb.addRow(i, suite, test, message)
        }
        println("Failing Tests".inBlue)
        println(tb)
        println("\nEnter BuildResult.lastResults for more information on failing tests\n\n".inRed)
      }

      reportCompilationFailures


    } else {

      println(("Build succeeded").inGreen)

      println("\nTimings by task".inBlue)
      println(timingsTable)
      if (results.exists(_.isTestResult)){
        reportSlowTestSuites
        reportSlowUnitTests
      } 
    }
  }

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

