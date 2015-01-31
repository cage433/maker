package maker.task.tasks

import java.io.File
import maker.project.{Module, BaseProject, Project}
import maker.task._
import maker.task.compile._
import maker.utils.FileUtils._
import maker.utils._
import maker.utils.os.Command
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter
import scala.collection.JavaConverters._
import scalaz.syntax.std.ToBooleanOps
import scala.collection.immutable.VectorBuilder

case class PackageJarTask(
  baseProject: BaseProject, 
  modules : Seq[Module],
  compilePhase : CompilePhase,
  version : Option[String]
) 
  extends Task with ToBooleanOps with EitherUtils
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

    import baseProject.{packageDir, packageJar, sourcePackageJar, docPackageJar, docOutputDir}
    import BuildJar.{build => buildJar}

    if (!packageDir.exists)
      packageDir.mkdirs

    val result = buildJar(
                    packageJar(compilePhase, version),
                    modules.map(_.outputDir(compilePhase)) ++ modules.map(_.resourceDir(compilePhase))
                  ) andThen
                  buildJar(
                    sourcePackageJar(compilePhase, version),
                    modules.flatMap(_.sourceDirs(compilePhase))
                  ) andThen
                  buildJar(
                    docPackageJar,
                    docOutputDir :: Nil
                  )
    result match {
      case Right(_) => 
        DefaultTaskResult(this, true, sw)
      case Left(error) => 
        DefaultTaskResult(this, false, sw, message = Some(error))
    }

  }
}

