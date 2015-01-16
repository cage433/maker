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

    def jarCommand(jarFile : File, updateOrCreate: String, baseDir: File) = {
      Command(
        module.props.Jar().getAbsolutePath, 
        updateOrCreate, 
        jarFile.getAbsolutePath,
        "-C", baseDir.getAbsolutePath, "."
      )
    }

    def jarDirectoriesCommands(jarFile : File, directories : Seq[File]) = {
      directories.filter(_.exists) match {
        case Nil => Nil
        case head :: tail => 
          jarCommand(jarFile, "cf", head) :: tail.map(jarCommand(jarFile, "uf", _))
      }
    }
    val cmds = {
      val modules = if (includeUpstreamModules)
        module :: module.allUpstreamModules
      else 
        List(module)


      val classJarCommands = jarDirectoriesCommands(
        module.packageJar(compilePhase),
        modules.map(_.outputDir(compilePhase)) ::: modules.map(_.resourceDir(compilePhase))
      )

      val sourceJarCommands = jarDirectoriesCommands(
        module.sourcePackageJar(compilePhase),
        modules.flatMap(_.sourceDirs(compilePhase))
      )
      classJarCommands ::: sourceJarCommands
    }

    cmds.find(_.exec != 0) match {
      case Some(failingCommand) =>
        DefaultTaskResult(this, false, sw, message = Some(failingCommand.savedOutput))
      case None =>
        DefaultTaskResult(this, true, sw)
    }
  }
}
