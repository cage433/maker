package maker.task.compile

import maker.project._
import maker.task.{Task, TaskResult}
import maker.utils.FileUtils._
import maker.utils.RichString._
import maker.utils._
import org.apache.commons.io.FileUtils._
import maker.task.TaskResult._
import sbt.compiler.CompileFailed
import maker.task.tasks.UpdateTask
import xsbti.{Problem, Severity}
import java.io.{BufferedWriter, File}
import xsbti.api.Compilation
import javax.sound.sampled.Line
import com.sun.org.apache.bcel.internal.classfile.Unknown
import maker.ScalaVersion

case class CompileTask(
  rootProject : ProjectTrait, 
  val module : Module, 
  scalaVersion : ScalaVersion,
  phase : CompilePhase
) 
  extends Task
{
  
  def upstreamTasks = {
    phase match {
      case SourceCompilePhase => 
        module.immediateUpstreamModules.map(CompileTask(rootProject, _, scalaVersion, SourceCompilePhase)) ++ 
          List(UpdateTask(rootProject, scalaVersion))
      case _ => 
          CompileTask(rootProject, module, scalaVersion, SourceCompilePhase) +: module.testModuleDependencies.map(CompileTask(rootProject, _, scalaVersion, phase))
    }
  }

  private def successfulResult(sw : Stopwatch, state : CompilationState) = CompileTaskResult(
    this, succeeded = true, 
    stopwatch = sw, 
    state = state
  )

  def compilationRequired(upstreamTaskResults : Iterable[TaskResult]) = {
    def hasDeletedSourceFiles = module.sourceFilesDeletedSinceLastCompilation(scalaVersion, phase).nonEmpty

    def upstreamCompilation = upstreamTaskResults.exists{
      case r : CompileTaskResult if r.state != CompilationNotRequired => true
      case _ => false
    }

    def modificationSinceLastCompilation = (module.lastSourceModifcationTime(phase), module.lastCompilationTime(scalaVersion, phase)) match {
      case (Some(t1), Some(t2)) => t1 > t2
      case _ => true
    }
    def lastCompilationFailed = module.lastCompilationFailed(scalaVersion, phase)
    hasDeletedSourceFiles ||  upstreamCompilation || modificationSinceLastCompilation || lastCompilationFailed
  }


  def exec(upstreamTaskResults : Iterable[TaskResult], sw : Stopwatch) : TaskResult = {

    if (module.sourceFiles(phase).isEmpty){
      cleanRegularFilesLeavingDirectories(module.classDirectory(scalaVersion, phase))
      module.compilationCacheFile(scalaVersion, phase).delete
      return successfulResult(sw, CompilationNotRequired)
    }
    if (compilationRequired(upstreamTaskResults)){
      module.compilerName match {
        case "zinc" => 
          val exitCode = ZincCompile(rootProject, module, phase, scalaVersion)
          if (exitCode == 0)
            successfulResult(sw, CompilationSucceeded)
          else {
            module.markCompilatonFailure(scalaVersion, phase)
            CompileTask.appendCompileOutputToTopLevel(module, scalaVersion, phase)
            CompileTaskResult(
              this, succeeded = false,
              stopwatch = sw, state = CompilationFailed("compilation failure")
            )
          }
        case "dummy-test-compiler" =>
          DummyCompileTask(module, phase, scalaVersion).exec
          successfulResult(sw, CompilationSucceeded)

      }
    } else {
      successfulResult(sw, CompilationNotRequired)
    }
  }


  def name = phase.toString 

}

case class CompilationFailedInfo(e : CompileFailed) {
  def failingFiles = e.problems.toList.map(_.position.sourceFile).filter(_.isDefined).map(_.get).distinct
  private def toString_(files : List[String]) = {
    val b = new StringBuffer
    b.append("Compilation failed for\n")
    b.append(files.mkString("\n"))
    b.toString
  }
}


object CompileTask{
  def CALL_TO_COMPILER = "CALL TO SCALA COMPILER"

  def reportOnCompilationErrors(taskResults : List[TaskResult]){
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

    val failures : List[(Module, String, String, String)] = taskResults.collect{
      case CompileTaskResult(
        task : CompileTask,
        false,
        _,
        _,
        Some(compFailed),
        _,
        _
      ) =>
        val severeProblems = compFailed.problems.filter(_.severity == Severity.Error)
        severeProblems.map{
          prob =>
            val (sourceFile, lineNo) = position(prob)
            (task.module, prob.message, sourceFile, lineNo)
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
      println("Compiler errors".inBlue)
      println(tb)
    }
  }

  def appendCompileOutputToTopLevel(module : Module, scalaVersion : ScalaVersion, phase : CompilePhase) = synchronized {
    withFileAppender(module.topLevelCompilationErrorsFile){
      writer : BufferedWriter =>
        // Vim bug prevents all error being shown unless 
        // there is a blank line initially
        writer.println("")
        module.moduleCompilationErrorsFile(scalaVersion, phase).readLines.foreach(writer.println)
    }
  }
}

case class CompileTaskResult(
  task : CompileTask, 
  succeeded : Boolean, 
  stopwatch : Stopwatch,
  state : CompilationState,
  maybeCompileFailed : Option[CompileFailed] = None,
  message : Option[String] = None, 
  exception : Option[Throwable] = None
) extends TaskResult
