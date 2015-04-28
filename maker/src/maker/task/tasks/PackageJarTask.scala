package maker.task.tasks

import java.io.File
import maker.project._
import maker.task._
import maker.task.compile._
import maker.utils.FileUtils._
import maker.utils._
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter
import scala.collection.JavaConverters._
import scala.collection.immutable.VectorBuilder
import scala.tools.nsc.io.Jar
import maker.ScalaVersion

case class PackageJarTask(
  project: Project, 
  version : Option[String],
  scalaVersion : ScalaVersion
) 
  extends Task with EitherPimps
{

  def name = "Package Main Jar"

  def upstreamTasks = DocTask(project, scalaVersion) :: Nil

  // TODO - find out why this is synchronized
  def exec(results: Iterable[TaskResult], sw: Stopwatch) = synchronized {

    import project.{packageDir, packageJar, sourcePackageJar, docPackageJar, docOutputDir}
    import BuildJar.{build => buildJar}

    if (!packageDir.exists)
      packageDir.mkdirs

    val modules = project.upstreamModules
    val result = buildJar(
                    packageJar(version),
                    modules.map(_.classDirectory(scalaVersion, SourceCompilePhase)) ++ modules.map(_.resourceDir(SourceCompilePhase))
                  ) andThen
                  buildJar(
                    sourcePackageJar(version),
                    modules.flatMap(_.sourceDirs(SourceCompilePhase))
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

