package maker.task.tasks

import maker.project.BaseProject
import maker.utils.{Stopwatch, EitherPimps}
import maker.task.{Task, TaskResult}
import maker.Resource

case class DownloadScalaLibs(baseProject : BaseProject) extends Task with EitherPimps{
  def name = "Download scala libs"
  def exec(results : Iterable[TaskResult] = Nil, sw : Stopwatch) : TaskResult = {
    import baseProject.scalaVersion
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
      null
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
//    scalaResources.foldLeft(Right(Unit)){
//      case (previousResult, nextResource) => 
//        previousResult.flatMap{
//          _ => 
//            nextResource.downloadDirectory
//        }
//    }
    null
  }

  def upstreamTasks : Iterable[Task] = Nil
}
