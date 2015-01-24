package maker.task.tasks

import java.io.File
import maker.project.Module
import maker.task._
import maker.task.compile._
import maker.utils.FileUtils._
import maker.utils.{Stopwatch, Int}
import maker.utils.os.Command
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter
import scala.collection.JavaConverters._
import scalaz.syntax.std.ToBooleanOps
import scala.collection.immutable.VectorBuilder

case class PackageJarTask(
  module: Module, 
  compilePhase : CompilePhase,
  includeUpstreamModules : Boolean 
) 
  extends Task with ToBooleanOps 
{
  def baseProject = module
  def name = compilePhase match {
    case SourceCompilePhase => "Package Main Jar"
    case TestCompilePhase => "Package Test Jar"
  }
  def upstreamTasks = {
    var tasks = new VectorBuilder[Task]()
    tasks += CompileTask(module, compilePhase)
    if (!includeUpstreamModules)
      tasks ++= module.immediateUpstreamModules.map(PackageJarTask(_, compilePhase, includeUpstreamModules = false))
    if (compilePhase == SourceCompilePhase)
      tasks += DocTask(module)

    tasks.result
  }

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
      val docJarCommands = jarDirectoriesCommands(
        module.docPackageJar,
        List(module.docOutputDir)
      )
      classJarCommands ::: sourceJarCommands ::: docJarCommands
    }

    cmds.find(_.exec != 0) match {
      case Some(failingCommand) =>
        DefaultTaskResult(this, false, sw, message = Some(failingCommand.savedOutput))
      case None =>
        DefaultTaskResult(this, true, sw)
    }
  }
}
