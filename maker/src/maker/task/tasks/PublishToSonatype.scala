package maker.task.tasks

import maker.project.Project
import maker.task.{Task, TaskResult, DefaultTaskResult}
import maker.utils._
import scala.util.{Either, Left, Right}
import java.io._
import maker.utils.FileUtils._
import spray.json._
import DefaultJsonProtocol._
import scala.xml.XML
import org.scalatest.Failed
import org.apache.http.auth.{AuthScope, UsernamePasswordCredentials}
import org.apache.http.impl.client.{DefaultHttpClient, BasicCredentialsProvider}
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.methods.{HttpPost, HttpGet}
import org.apache.http.client.HttpClient
import org.apache.http.{HttpStatus, HttpResponse, HttpHost}
import org.apache.http.entity._
import org.apache.http.conn.params.ConnRoutePNames
import org.apache.http.util.EntityUtils
import java.util.jar.{JarOutputStream, JarEntry}
import scala.collection.immutable.Nil
import maker.ScalaVersion

case class PublishToSonatype(project : Project, version : String) 
  extends Task 
  with SonatypeTask
  with EitherPimps
{
  val sonatypeRepository = "https://oss.sonatype.org/service/local"
           
  type JsonResponse = String
  type StagingRepo = String
  def name = s"Publish $project to Sonatype"

  val system = new AetherSystem(project.resourceCacheDirectory)
  def upstreamTasks = 
    PublishLocalTask(project, version, signArtifacts = true) :: Nil

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
      case Right(_) => 
        DefaultTaskResult(this, true, sw)
    }
  }

  private def makeBundle(tmpDir : File) : Either[ErrorMessage, File] = {

    val bundleJar = file(System.getenv("HOME"), "tmp", "bundle.jar")

    BuildJar.build(
      bundleJar, 
      project.publishLocalDir(version) :: Nil,
      fileFilter = {f => 
        println(s"File $f")
        Seq("jar", "pom", "asc").exists(f.extension == _)}
    ).map{
      _ => bundleJar
    }
  }

  private def uploadToSonatype(bundle : File) : Either[ErrorMessage, JsonResponse] = {
    Post(
      s"${sonatypeRepository}/staging/bundle_upload",
      new FileEntity(bundle, ContentType.create("application/java-archive"))
    ).map{
      responseString => 
      JsonParser(responseString).convertTo[RepoUris].repoIdentifier
    }
  }

  private def waitTillRepoIsClosed(stagingRepo : String) : Either[ErrorMessage, Unit] = {
    var isClosed = false
    var numTriesLeft = 10
    while(!isClosed && numTriesLeft > 0){
      Get(s"${sonatypeRepository}/staging/repository/$stagingRepo"){
        response => 
          val xml = XML.load(response.getEntity.getContent)
          val status = (xml \ "type").text
          if (status == "closed")
            isClosed = true
          else
            Thread.sleep(5000)
          numTriesLeft -= 1
      }
    }
 
    if (isClosed)
      Right(())
    else 
      Left("Couldn't close")
  }




  case class RepoUris(repositoryUris : Seq[String]){
    require(repositoryUris.size == 1, s"expected a single uri, got $repositoryUris")
    def repoIdentifier = repositoryUris(0).split('/').last
  }

  implicit val jsonReposFormatter = jsonFormat1(RepoUris)
 
  private def releaseStagingRepo(tmpDir : File, stagingRepo : StagingRepo) : Either[ErrorMessage, Unit] = {
    Post(
      s"${sonatypeRepository}/staging/bulk/promote",
      new StringEntity(
        s"""{"data":{"stagedRepositoryIds":["$stagingRepo"],"description":""}}""",
        ContentType.APPLICATION_JSON
      )
    ).map {
      _ => ()
    }
  }

}
