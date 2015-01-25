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

  // TODO - find out why this is synchronized
  def exec(results: Iterable[TaskResult], sw: Stopwatch) = synchronized {
    if (!module.packageDir.exists)
      module.packageDir.mkdirs

    def jarDirectories(jarFile : File, directories : Seq[File]) = {
      jarFile.delete

      def jarCommand(directory : File) = {
        Command(
          module.props.Jar().getAbsolutePath, 
          if (jarFile.exists) "uf" else "cf",
          jarFile.getAbsolutePath,
          "-C", directory.getAbsolutePath, "."
        ).withSavedOutput
      }

      // Add contents of each directory to the jar. Stopping
      // at the first failure
      directories.foldLeft(None : Option[String]){
        case (maybeFailure, directory) => 

          maybeFailure match {
            case e : Some[_] => e
            case None => {
              val cmd = jarCommand(directory)
              if (cmd.exec == 0)
                None
              else
                Some(cmd.savedOutput)
            }
          }

      }
    }

    val modules = if (includeUpstreamModules)
      module.allUpstreamModules
    else 
      List(module)

    val maybeError = jarDirectories(
      module.packageJar(compilePhase),
      modules.map(_.outputDir(compilePhase)) ::: modules.map(_.resourceDir(compilePhase))
    ) orElse jarDirectories(
      module.sourcePackageJar(compilePhase),
      modules.flatMap(_.sourceDirs(compilePhase))
    )  orElse jarDirectories(
      module.docPackageJar,
      List(module.docOutputDir)
    )

    maybeError match {
      case Some(error) =>
        DefaultTaskResult(this, false, sw, message = Some(error))
      case None =>
        DefaultTaskResult(this, true, sw)
    }
  }
}
