package maker.build

import java.io.File
import maker.task.TaskResult._
import maker.utils.Stopwatch
import maker.utils.Implicits.RichString._
import maker.MakerProps
import maker.task.compile.CompilationInfo
import java.util.concurrent.atomic.AtomicReference
import maker.task.compile.CompileTask
import maker.task.test.RunUnitTestsTask
import maker.utils.Implicits.RichIterable._
import maker.task.TaskResult
import maker.task.test.TestResults


case class BuildResult(
  name : String,
  results : List[TaskResult],
  graph : Dependency.Graph,
  clockTime : Long
) {

  self =>

  def testResults() : TestResults = {
    self.results.map(_.info).collect{
      case Some(tr : TestResults) => tr
    }.foldLeft(TestResults.EMPTY)(_++_)
  }
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
      buffer.append(name + " succeeded in " + Stopwatch.milliToHumanString(clockTime))
      buffer.toString inBlue
    } else {
      val firstFailure : TaskResult = maybeFirstFailure.get
      buffer.append(name + " failed in " + Stopwatch.milliToHumanString(clockTime) + ".\nFirst failing upstream task is\n\n")
      buffer.append(firstFailure + "")
      reportNumberOfScalaFilesCompiled
      buffer.toString inRed
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
    result(i).info.foreach(println)
  }
  def info{
    result.maybeFirstFailure.foreach(_.info.foreach(println))
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

