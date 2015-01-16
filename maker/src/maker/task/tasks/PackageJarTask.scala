package maker.task.tasks

import java.io.File
import maker.project.Module
import maker.task.{DefaultTaskResult, SingleModuleTask, TaskResult}
import maker.task.compile._
import maker.utils.FileUtils._
import maker.utils.{Stopwatch, Int}
import maker.utils.os.Command
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter
import scala.collection.JavaConverters._
import scalaz.syntax.std.ToBooleanOps

case class PackageJarTask(
  module: Module, 
  compilePhase : CompilePhase,
  includeUpstreamModules : Boolean 
) 
  extends SingleModuleTask(module) with ToBooleanOps 
{
  def name = compilePhase match {
    case SourceCompilePhase => "Package Main Jar"
    case TestCompilePhase => "Package Test Jar"
  }
  def upstreamTasks = if (includeUpstreamModules)
    Vector(CompileTask(module, compilePhase))
  else
    CompileTask(module, compilePhase) :: 
      module.immediateUpstreamModules.map(PackageJarTask(_, compilePhase, includeUpstreamModules = false))

  def exec(results: Iterable[TaskResult], sw: Stopwatch) = synchronized {
    doPackage(results, sw)
  }

  private def doPackage(results: Iterable[TaskResult], sw: Stopwatch) = {
    if (!module.packageDir.exists)
      module.packageDir.mkdirs

    def jarCommand(updateOrCreate: String, baseDir: File) = {
      Command(
        module.props.Jar().getAbsolutePath, 
        updateOrCreate, 
        module.packageJar(compilePhase).getAbsolutePath,
        "-C", baseDir.getAbsolutePath, "."
      )
    }

    val cmds = {
      val modules = if (includeUpstreamModules)
        module :: module.allUpstreamModules
      else 
        List(module)

      val dirs = modules.flatMap{
        m => 
          Vector(m.outputDir(compilePhase), m.resourceDir(compilePhase))
      }.filter(_.exists)

      dirs match {
        case Nil => Nil
        case head :: tail => 
          jarCommand("cf", head) :: tail.map(jarCommand("uf", _))
      }
    }

    cmds.find(_.exec != 0) match {
      case Some(failingCommand) =>
        DefaultTaskResult(this, false, sw, message = Some(failingCommand.savedOutput))
      case None =>
        DefaultTaskResult(this, true, sw)
    }
  }
}
