package maker.task.tasks

import java.io.File
import maker.project.{Module, BaseProject, Project}
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
  baseProject: BaseProject, 
  modules : Seq[Module],
  compilePhase : CompilePhase
) 
  extends Task with ToBooleanOps 
{

  def name = compilePhase match {
    case SourceCompilePhase => "Package Main Jar"
    case TestCompilePhase => "Package Test Jar"
  }

  def upstreamTasks = {
    compilePhase match {
      case SourceCompilePhase => 
        modules.map(DocTask(_))
      case TestCompilePhase => 
        modules.map(CompileTask(_, TestCompilePhase))
    }
  }

  // TODO - find out why this is synchronized
  def exec(results: Iterable[TaskResult], sw: Stopwatch) = synchronized {

    if (!baseProject.packageDir.exists)
      baseProject.packageDir.mkdirs

    def jarDirectories(jarFile : File, directories : Seq[File]) = {
      jarFile.delete

      def jarCommand(directory : File) = {
        Command(
          baseProject.props.Jar().getAbsolutePath, 
          if (jarFile.exists) "uf" else "cf",
          jarFile.getAbsolutePath,
          "-C", directory.getAbsolutePath, "."
        ).withSavedOutput
      }

      // Add contents of each directory to the jar. Stopping
      // at the first failure
      directories.filter(_.exists).foldLeft(None : Option[String]){
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

    val maybeError = 
      jarDirectories(
        baseProject.packageJar(compilePhase),
        modules.map(_.outputDir(compilePhase)) ++ modules.map(_.resourceDir(compilePhase))
      ) orElse 
      jarDirectories(
        baseProject.sourcePackageJar(compilePhase),
        modules.flatMap(_.sourceDirs(compilePhase))
      ) orElse 
      jarDirectories(
        baseProject.docPackageJar,
        List(baseProject.docOutputDir)
      )

    maybeError match {
      case Some(error) =>
        DefaultTaskResult(this, false, sw, message = Some(error))
      case None =>
        DefaultTaskResult(this, true, sw)
    }
  }
}

