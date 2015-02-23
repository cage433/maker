package maker.task.tasks

import maker.project.BaseProject
import maker.utils._
import maker.utils.FileUtils._
import maker.task.{Task, TaskResult, DefaultTaskResult}
import maker.{Resource, MakerConfig}
import maker.utils.http.HttpUtils
import java.io.{InputStream, FileOutputStream}
import maker.Resource._

class DownloadScalaLibs extends Task 
  with EitherPimps 
  with MakerConfig
{
  def name = "Download scala libs"
  def exec(results : Iterable[TaskResult] = Nil, sw : Stopwatch) : TaskResult = {
    val scalaVersion = config.getString("maker.project.scala.version")

    def requiresScalaReflect = {
      scalaVersion.split('.').toList match {
        case List("2", "9", _) => 
          false
          
        case List("2", "10", _) | List("2", "11", _) => 
          true

        case _ =>
          throw new IllegalStateException(s"Unsupported scala version $scalaVersion")
      }
    }
    def download(library : String, version : String) : Either[List[(Int, String)], Unit] = {
      val resource = Resource(
        "org.scala-lang", library, version, 
        downloadDirectory = Some(
          config.projectScalaLibDirectory
        )
      )
      resource.update() match {
        case ResourceAlreadyExists | ResourceDownloaded => 
          Right(Unit)
        case ResourceFailedToDownload(errors) => 
          Left(errors)
      }
    }

    val success = download("scala-library", scalaVersion) andThen
                  download("scala-compiler", scalaVersion) andThen (
                    if (requiresScalaReflect)
                      download("scala-reflect", scalaVersion)
                    else
                      Right(Unit)
                  )
    success match {
      case Left(errorMessage) =>
        DefaultTaskResult(this, succeeded = false, stopwatch = sw)
      case Right(_) => 
        DefaultTaskResult(this, succeeded = true, stopwatch = sw)
    }
  }

  def upstreamTasks : Iterable[Task] = Nil
}
