package maker.task.tasks

import maker.project.BaseProject
import maker.task.{Task, TaskResult, DefaultTaskResult}
import maker.utils._
import scala.util.{Either, Left, Right}
import java.io._
import maker.utils.FileUtils._
import spray.json._
import DefaultJsonProtocol._
import com.sun.xml.internal.txw2.Content
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

case class PublishToSonatype(baseProject : BaseProject, version : String) extends Task with EitherPimps{
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
      case Right(_) => 
        DefaultTaskResult(this, true, sw)
    }
  }

  private def makeBundle(tmpDir : File) : Either[ErrorMessage, File] = {
    import baseProject.{publishLocalPomDir, publishLocalJarDir}

    val bundleJar = file("/home/alex/tmp", "bundle.jar")

    BuildJar.build(bundleJar, publishLocalPomDir(version) :: publishLocalJarDir(version) :: Nil).map{
      _ => bundleJar
    }
  }

  private def uploadToSonatype(bundle : File) : Either[ErrorMessage, JsonResponse] = {
    Post(
      "/staging/bundle_upload", 
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
      Get(s"/staging/repository/$stagingRepo"){
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
    val req = new HttpGet(s"${sonatypeRepository}$path")
    req.addHeader("Content-Type", "application/xml")
    withHttpClient{ client =>
      val response = client.execute(req)
      if(response.getStatusLine.getStatusCode != HttpStatus.SC_OK) {
        throw new IOException(s"Failed to retrieve data from $path: ${response.getStatusLine}")
      }
      body(response)
    }
  }

  def Post(path:String, entity : AbstractHttpEntity) : Either[ErrorMessage, String] = {
    val req = new HttpPost(s"${sonatypeRepository}$path")
    println(s"Request = $req")
    req.setEntity(entity)
    withHttpClient{ client =>
      val response = client.execute(req)
      if (response.getStatusLine.getStatusCode == HttpStatus.SC_CREATED)
        Right(EntityUtils.toString(response.getEntity))
      else 
        Left(s"${response}")
    }
  }

  case class RepoUris(repositoryUris : Seq[String]){
    require(repositoryUris.size == 1, s"expected a single uri, got $repositoryUris")
    def repoIdentifier = repositoryUris(0).split('/').last
  }

  implicit val jsonReposFormatter = jsonFormat1(RepoUris)
 
  private def releaseStagingRepo(tmpDir : File, stagingRepo : StagingRepo) : Either[ErrorMessage, Unit] = {
    Post(
      "/staging/bulk/promote",
      new StringEntity(
        s"""{"data":{"stagedRepositoryIds":["$stagingRepo"],"description":""}}""",
        ContentType.APPLICATION_JSON
      )
    ).map {
      _ => ()
    }
  }

}
