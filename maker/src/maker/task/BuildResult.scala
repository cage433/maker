package maker.task

import java.io.File
import java.util.concurrent.atomic.AtomicReference
import maker.project.Module
import maker.task.compile._
import maker.task.TaskResult._
import maker.utils.FileUtils._
import maker.utils.RichIterable._
import maker.utils.RichString._
import maker.utils._
import sbt.compiler.CompileFailed
import xsbti.Position
import xsbti.Problem
import xsbti.Severity
import maker.task.tasks.RunUnitTestsTaskResult
import maker.task.tasks.UpdateTask
import maker.task.tasks.RunUnitTestsTask


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
    if (failed){
      println("Build failed".inRed)
      println
      TaskResult.reportOnFirstFailingTask(results)
      UpdateTask.reportOnUpdateFailures(results)
      RunUnitTestsTask.reportOnFailingTests(results)
      CompileTask.reportOnCompilationErrors(results)
    } else {

      println(("Build succeeded").inGreen)
      println

      FailingTests.clear()
      TaskResult.reportOnTaskTimings(results)
      RunUnitTestsTask.reportOnSlowTests(results)
    }
  }

  def testResults = {
    val testResultsList : List[MakerTestResults] = results.collect{
      case r : RunUnitTestsTaskResult => r.testResults
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

  def andThen(next : => BuildResult) : BuildResult = {
    if (succeeded) next else this
  }
 
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

}

object BuildResult{
  val lastResult : AtomicReference[Option[BuildResult]] = new AtomicReference(None)
  def last = LastResult(lastResult.get.get)
}

