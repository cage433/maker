package maker.task.compile

import maker.project.Module
import java.io.File
import maker.utils.FileUtils._
import maker.utils.Implicits.RichIterable._
import maker.utils.Implicits.RichString._
import maker.task.TaskResult
import sbt.inc.Analysis

trait CompilationState
case object CachedCompilation extends CompilationState 
case object CompilationSucceeded extends CompilationState
case object CompilationNotRequired extends CompilationState

case class CompilationFailed(error : String) extends CompilationState

case class CompilationInfo(task : CompileTask, state : CompilationState) {
  def module = task.module
  def phase = task.phase

  override def toString = {
    val b = new StringBuffer
    b.addLine("CompilationInfo")
    b.addLine("  Task :" + task)
    b.addLine("  State : " + state)
    b.toString
  }
}



