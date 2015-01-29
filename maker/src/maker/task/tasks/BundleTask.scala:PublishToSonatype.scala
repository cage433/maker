package maker.task.tasks

import maker.project.BaseProject
import maker.task.{Task, TaskResult, DefaultTaskResult}
import maker.utils.{Stopwatch, EitherUtils, FileUtils}
import scala.util.{Either, Left, Right}
import java.io.{File, IOException}
import maker.utils.os.{Command, CommandOutputHandler}
import maker.utils.FileUtils._
import spray.json._
import DefaultJsonProtocol._
import com.sun.xml.internal.txw2.Content
import xsbti.Exit
import scala.collection.immutable.Nil
import scala.xml.XML
import org.scalatest.Failed
import org.apache.http.auth.{AuthScope, UsernamePasswordCredentials}
import org.apache.http.impl.client.{DefaultHttpClient, BasicCredentialsProvider}
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.methods.{HttpPost, HttpGet}
import org.apache.http.client.HttpClient
import org.apache.http.{HttpStatus, HttpResponse, HttpHost}
import org.apache.http.entity.StringEntity
import org.apache.http.conn.params.ConnRoutePNames

case class PublishToSonatype(baseProject : BaseProject, version : String) extends Task with EitherUtils{
  import baseProject.props
  val Array(sonatypeUsername, sonatypePassword) = props.SonatypeCredentials().split(":")
  val sonatypeRepository = "https://oss.sonatype.org/service/local"
  val credentialHost = "oss.sonatype.org"
           
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

  val proxy = new HttpHost("127.0.0.1", 4128, "http")
  private def withHttpClient[U](body: HttpClient => U) : U = {

    val client = new DefaultHttpClient()
    client.getParams().setParameter(ConnRoutePNames.DEFAULT_PROXY, proxy)
    try {
      client.getCredentialsProvider.setCredentials(
        new AuthScope(credentialHost, AuthScope.ANY_PORT),
        new UsernamePasswordCredentials(sonatypeUsername, sonatypePassword)
      )
      body(client)
    }
    finally
      client.getConnectionManager.shutdown()
  }

  def Get[U](path:String)(body: HttpResponse => U) : U = {
    val config = RequestConfig.custom().setProxy(proxy).build()
    val req = new HttpGet(s"${sonatypeRepository}$path")
//    req.setConfig(config)
    req.addHeader("Content-Type", "application/xml")
    withHttpClient{ client =>
      val response = client.execute(req)
      if(response.getStatusLine.getStatusCode != HttpStatus.SC_OK) {
        throw new IOException(s"Failed to retrieve data from $path: ${response.getStatusLine}")
      }
      body(response)
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

  case class StagingRepositoryProfile(profileId:String, profileName:String, stagingType:String, repositoryId:String) {
    override def toString = s"[$repositoryId] status:$stagingType, profile:$profileName($profileId)"
    def isOpen = stagingType == "open"
    def isClosed = stagingType == "closed"
    def isReleased = stagingType == "released"

    def toClosed = StagingRepositoryProfile(profileId, profileName, "closed", repositoryId)
    def toReleased = StagingRepositoryProfile(profileId, profileName, "released", repositoryId)
  }
 
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
    if (cmd.exec != 0)
      Left("Exit code was " + cmd.savedOutput)
    else{
      Right(cmd.savedOutput)
    }
  }

  private def waitTillRepoIsClosed(stagingRepo : String) : Either[ErrorMessage, Unit] = {
    var isClosed = false
    var numTriesLeft = 10
    while(!isClosed && numTriesLeft > 0){
      Get(s"/staging/repository/$stagingRepo"){
        response => 
          val xml = XML.load(response.getEntity.getContent)
          val status = (xml \ "type").text
          if (status == "closed")
            isClosed = true
          else
            Thread.sleep(5000)
          println(s"status = $status")
          numTriesLeft -= 1

      }
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
    error.toLeft(bundleJar)
  }
}
