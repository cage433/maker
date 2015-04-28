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

abstract class CompileTask(
  rootProject : ProjectTrait, 
  val module : Module, 
  majorScalaVersion : String
) 
  extends Task
{
  
  def phase : CompilePhase

  val modulePhase = ModuleCompilePhase(module, phase)

  private def successfulResult(sw : Stopwatch, state : CompilationState) = CompileTaskResult(
    this, succeeded = true, 
    stopwatch = sw, 
    state = state
  )

  def compilationRequired(upstreamTaskResults : Iterable[TaskResult]) = {
    def hasDeletedSourceFiles = modulePhase.sourceFilesDeletedSinceLastCompilation(majorScalaVersion).nonEmpty

    def upstreamCompilation = upstreamTaskResults.exists{
      case r : CompileTaskResult if r.state != CompilationNotRequired => true
      case _ => false
    }

    def modificationSinceLastCompilation = (modulePhase.lastSourceModifcationTime, modulePhase.lastCompilationTime) match {
      case (Some(t1), Some(t2)) => t1 > t2
      case _ => true
    }
    def lastCompilationFailed = modulePhase.lastCompilationFailed()
    hasDeletedSourceFiles ||  upstreamCompilation || modificationSinceLastCompilation || lastCompilationFailed
  }


  def exec(upstreamTaskResults : Iterable[TaskResult], sw : Stopwatch) : TaskResult = {

    if (modulePhase.sourceFiles.isEmpty){
      cleanRegularFilesLeavingDirectories(module.classDirectory(majorScalaVersion, phase))
      modulePhase.compilationCacheFile.delete
      return successfulResult(sw, CompilationNotRequired)
    }
    if (compilationRequired(upstreamTaskResults)){
      module.compilerName match {
        case "zinc" => 
          val exitCode = ZincCompile(rootProject, module, phase, majorScalaVersion)
          if (exitCode == 0)
            successfulResult(sw, CompilationSucceeded)
          else {
            modulePhase.markCompilatonFailure()
            CompileTask.appendCompileOutputToTopLevel(modulePhase)
            CompileTaskResult(
              this, succeeded = false,
              stopwatch = sw, state = CompilationFailed("compilation failure")
            )
          }
        case "dummy-test-compiler" =>
          DummyCompileTask(module, phase, majorScalaVersion).exec
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


case class SourceCompileTask(rootProject: ProjectTrait, override val module :Module, majorScalaVersion : String) 
  extends CompileTask(rootProject, module, majorScalaVersion){
  def upstreamTasks = {
    module.immediateUpstreamModules.map(SourceCompileTask(rootProject, _, majorScalaVersion)) ++ 
      List(UpdateTask(rootProject, majorScalaVersion, forceSourceUpdate = false))
  }
  def phase = SourceCompilePhase
} 

case class TestCompileTask(rootProject : ProjectTrait, override val module : Module, majorScalaVersion : String) 
  extends CompileTask(rootProject, module, majorScalaVersion){
  def upstreamTasks: Seq[Task] =
      SourceCompileTask(rootProject, module, majorScalaVersion) +: module.testModuleDependencies.map(TestCompileTask(rootProject, _, majorScalaVersion))

  def phase = TestCompilePhase
}

object CompileTask{
  def CALL_TO_COMPILER = "CALL TO SCALA COMPILER"
  //def apply(module : Module, phase : CompilePhase) : CompileTask = {
    //phase match{
      //case SourceCompilePhase => SourceCompileTask(module)
      //case TestCompilePhase => TestCompileTask(module)
    //}
  //}

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

  def appendCompileOutputToTopLevel(modulePhase : ModuleCompilePhase) = synchronized {
    withFileAppender(modulePhase.module.topLevelCompilationErrorsFile){
      writer : BufferedWriter =>
        // Vim bug prevents all error being shown unless 
        // there is a blank line initially
        writer.println("")
        modulePhase.moduleCompilationErrorsFile.readLines.foreach(writer.println)
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
