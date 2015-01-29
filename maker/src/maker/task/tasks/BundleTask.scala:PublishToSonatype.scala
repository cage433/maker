package maker.task.tasks

import maker.project.BaseProject
import maker.task.{Task, TaskResult, DefaultTaskResult}
import maker.utils.{Stopwatch, EitherUtils, FileUtils}
import scala.util.{Either, Left, Right}
import java.io.File
import maker.utils.os.{Command, CommandOutputHandler}
import maker.utils.FileUtils._
import spray.json._
import DefaultJsonProtocol._
import com.sun.xml.internal.txw2.Content
import xsbti.Exit
import scala.collection.immutable.Nil
import scala.xml.XML


case class PublishToSonatype(baseProject : BaseProject, version : String) extends Task with EitherUtils{
  type ErrorMessage = String
  type JsonResponse = String
  type StagingRepo = String
  def name = s"Publish $baseProject to Sonatype"

  def upstreamTasks = 
    PublishLocalTask(baseProject, baseProject.allUpstreamModules, version, signArtifacts = true) :: Nil

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    FileUtils.withTempDir{
      dir => 
        for{
          bundleJar <- makeBundle(dir)
          stagingRepo <- uploadToSonatype(bundleJar)
          _ <- waitTillRepoIsClosed(stagingRepo)
        } yield{
          releaseStagingRepo(dir, stagingRepo)
        }
    } match {
      case Left(error) => 
        DefaultTaskResult(this, false, sw, message = Some(error))
      case Right(output) => 
        println(output)
        DefaultTaskResult(this, true, sw)
    }
  }

  case class RepoUris(repositoryUris : Seq[String]){
    require(repositoryUris.size == 1, s"expected a single uri, got $repositoryUris")
    def repoIdentifier = repositoryUris(0).split('/').last
  }

  case class PayloadData(stagedRepositoryIds : Seq[String], description : String)
  case class ReleasePayload(data : PayloadData)
  implicit val jsonReposFormatter = jsonFormat1(RepoUris)
  implicit val payloadDataFormatter = jsonFormat2(PayloadData)
  implicit val releasePayloadFormatter = jsonFormat1(ReleasePayload)

  private def releaseStagingRepo(tmpDir : File, stagingRepo : StagingRepo) : Either[ErrorMessage, Unit] = {
    val payload = ReleasePayload(PayloadData(List(stagingRepo), ""))
    val payloadJson = payload.toJson.compactPrint
    val cmd = Command(
      CommandOutputHandler(),
      None,
      true,
      "curl", 
      "-u", "cage433:zoltan",
      "-o", "out.txt",
      "-w", "%{http_code}",
      "-H", "Accept: application/json", 
      "-H", "Content-Type: application/json",
      "-d", payloadJson,
      "https://oss.sonatype.org/service/local/staging/bulk/promote"
    ).withSavedOutput
    println(cmd)
    if (cmd.exec != 0)
      Left("Exit code was " + cmd.savedOutput)
    else{
      println(s"Saved output ${cmd.savedOutput}")
      Right(cmd.savedOutput)
    }
  }

  private def waitTillRepoIsClosed(stagingRepo : String) : Either[ErrorMessage, Unit] = {
    var isClosed = false
    var numTriesLeft = 10
    while(!isClosed && numTriesLeft > 0){
      println("Trying to see if repo closed")
      val cmd = Command(
        CommandOutputHandler.NULL,
        None,
        false,
        "curl",
        "-u", "cage433:zoltan",
        s"https://oss.sonatype.org/service/local/staging/repository/$stagingRepo"
      ).withSavedOutput
      cmd.exec
      println(s"***${cmd.savedOutput}****")
      val xml = XML.loadString(cmd.savedOutput)
      val status = (xml \ "type").text
      println(s"status = $status")
      if (status == "closed")
        isClosed = true
      numTriesLeft -= 1
      Thread.sleep(5000)
    }
    if (isClosed)
      Right(())
    else 
      Left("Couldn't close")
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

    if (cmd.exec == 0){
      Right(JsonParser(cmd.savedOutput).convertTo[RepoUris].repoIdentifier)
    } else{
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
