package maker.task.tasks

import maker.project.BaseProject
import maker.utils.{Stopwatch, EitherPimps}
import maker.utils.FileUtils._
import maker.task.{Task, TaskResult, DefaultTaskResult}
import maker.{Resource, MakerConfig}
import maker.utils.http.HttpUtils
import java.io.{InputStream, FileOutputStream}

class DownloadScalaLibs extends Task 
  with EitherPimps 
  with MakerConfig
{
  def name = "Download scala libs"
  def exec(results : Iterable[TaskResult] = Nil, sw : Stopwatch) : TaskResult = {
    val scalaVersion = config.getString("maker.project.scala.version")
    val resolver = config.getString("maker.project.scala.resolver")

    var scalaResources = List(
      Resource("org.scala-lang", "scala-library", scalaVersion),
      Resource("org.scala-lang", "scala-compiler", scalaVersion),
      Resource("org.scala-lang", "jline", scalaVersion)
    )
    scalaVersion.split('.').toList match {
      case List("2", "9", _) =>
      case List("2", "10", _) | List("2", "11", _) =>
        scalaResources :+= Resource("org.scala-lang", "scala-reflect", scalaVersion)
    }

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
    def download(library : String) : Either[String, Unit] = {
      val url = s"$resolver/org.scala-lang/$library/$scalaVersion/$library-$scalaVersion.jar"
      val jarFile = file(
        config.getString("maker.project.scala.library-directory"), 
        s"org.scala-lang-$library-$scalaVersion.jar"
      )
      new HttpUtils().Get(url){
        response => 
          val entity = response.getEntity()
          Option(entity).map{
            _ => 
              val fos = new FileOutputStream(jarFile)
              entity.writeTo(fos)
              fos.close
              Unit
          }.toRight("http get of $url returned null")
      }
    }

    val success : Either[String, Unit] = for {
      _ <- download("scala-library")
      _ <- download("scala-compiler")
      _ <- download("jline")
    } yield {
      _ : Unit => 
        if (requiresScalaReflect)
          download("scala-reflect")
        else
          Right(Unit)
    } 

    success match {
      case Left(errorMessage) =>
        DefaultTaskResult(this, succeeded = false, stopwatch = sw)
      case Right(_) => 
        DefaultTaskResult(this, succeeded = true, stopwatch = sw)
    }
  }

  def upstreamTasks : Iterable[Task] = Nil
}
