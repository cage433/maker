package maker.task.tasks

import maker.project.BaseProject
import maker.task.{Task, TaskResult, DefaultTaskResult}
import maker.utils.{Stopwatch, EitherUtils, FileUtils}
import scala.util.{Either, Left, Right}
import java.io.File
import maker.utils.os.{Command, CommandOutputHandler}
import maker.utils.FileUtils._
import spray.json._


case class PublishToSonatype(baseProject : BaseProject, version : String) extends Task with EitherUtils{
  type ErrorMessage = String
  type JsonResponse = String
  def name = s"Publish $baseProject to Sonatype"

  def upstreamTasks = 
    PublishLocalTask(baseProject, baseProject.allUpstreamModules, version, signArtifacts = true) :: Nil

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    val repo = FileUtils.withTempDir{
      dir => 
        for{
          bundleJar <- makeBundle(dir)
          stagingRepoJson <- uploadToSonatype(bundleJar)
        } yield{
          println(s"Returning $stagingRepoJson")
          stagingRepoJson
        }
    }
    println(s"repo = $repo")
    DefaultTaskResult(this, true, sw)
  }

  private def releaseStagingRepo(stagingRepoJson : String) : Either[ErrorMessage, Unit] = {
    val json = 
    null
  }
  private def uploadToSonatype(bundle : File) : Either[ErrorMessage, JsonResponse] = {
    val cmd = Command(
      CommandOutputHandler.NULL,
      None,
      false,
      "curl", 
      "-u", "cage433:zoltan",
      "-F", s"filename=@${bundle.getAbsolutePath}",
      "https://oss.sonatype.org/service/local/staging/bundle_upload"
    ).withSavedOutput

    println(s"cmd = $cmd")
    if (cmd.exec == 0){
      println(s"Right ${cmd.savedOutput}")
      Right(cmd.savedOutput)
    } else{
      println(s"Left ${cmd.savedOutput}")
      Left(cmd.savedOutput)
    }
  }

  private def makeBundle(tmpDir : File) : Either[ErrorMessage, File] = {
    import baseProject.{publishLocalPomDir, publishLocalJarDir}

    val bundleJar = file("/home/alex/tmp/", "bundle.jar")

    def addDirectoryToBundle(directory : File) : Option[ErrorMessage] =  {
      val cmd = Command(
        baseProject.props.Jar().getAbsolutePath,
        if (bundleJar.exists) "uf" else "cf",
        bundleJar.getAbsolutePath,
        "-C",
        directory.getAbsolutePath,
        "."
      ).withNoOutput
      if (cmd.exec == 0)
        None
      else
        Some(cmd.savedOutput)
    }

    val error = addDirectoryToBundle(publishLocalPomDir(version)) orElse
      addDirectoryToBundle(publishLocalJarDir(version))
    println(s"Error was $error")
    error.toLeft(bundleJar)
  }
}
