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
  module: BaseProject, 
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
    val tasks = new VectorBuilder[Task]()
    val docOrCompile : Module => Task = compilePhase match {
      case SourceCompilePhase => 
        DocTask(_)
      case TestCompilePhase => 
        CompileTask(_, TestCompilePhase)
    }
    module match {
      case m : Module => 
        tasks += docOrCompile(m)
      case p : Project => 
        tasks ++= p.immediateUpstreamModules.map(docOrCompile(_))
    }
    (baseProject, includeUpstreamModules) match {
      case (_ : Project, _) | (_ : Module, false) => 
        tasks ++= module.immediateUpstreamModules.map(PackageJarTask(_, compilePhase, includeUpstreamModules = false))
      case _ => 
    }

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

    val modules = (baseProject, includeUpstreamModules) match {
      case (_ : Project, _) | (_, true) => 
        module.allUpstreamModules
      case (m : Module, false) => 
        Vector(m)
    }

    val maybeError = jarDirectories(
      module.packageJar(compilePhase),
      modules.map(_.outputDir(compilePhase)) ++ modules.map(_.resourceDir(compilePhase))
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

object PackageJarTask{
  def apply2(baseProject: BaseProject, 
            compilePhase : CompilePhase,
            includeUpstreamModules : Boolean) = {
    val modules = (baseProject, includeUpstreamModules) match {
      case (_ : Project, _) | (_ : Module, true) => 
        baseProject.allUpstreamModules
      case (m : Module, false) => 
        Vector(m)
    }
    //PackageJarTask(baseProject, modules, compilePhase)
    null
  }

}
