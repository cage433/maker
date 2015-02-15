package maker.task.tasks

import maker.project.BaseProject
import maker.utils.Stopwatch
import maker.task.{Task, TaskResult}
import maker.Resource

case class DownloadScalaLibs(baseProject : BaseProject) extends Task{
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
