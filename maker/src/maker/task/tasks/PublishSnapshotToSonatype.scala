package maker.task.tasks

import maker.project.{ProjectTrait, Project}
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

case class PublishSnapshotToSonatype(project : Project, version : String) extends Task
with SonatypeTask with EitherPimps{
  val sonatypeRepository = "https://oss.sonatype.org/service/local"
           
  def name = s"Publish $project snapshot to Sonatype"

  def upstreamTasks = 
    PublishLocalTask(project, version, signArtifacts = false) :: Nil

  def exec(results : Iterable[TaskResult], sw : Stopwatch) = {
    val toUpload : Seq[(File, ContentType)] = {
      (project.publishLocalPomFile(version), ContentType.APPLICATION_XML) +: {
      findJars(project.publishLocalJarDir(version)).map((_, ContentType.DEFAULT_BINARY))
    }
    }

    EitherPimps.mapOrErrorLeft(toUpload, {
      fc : (File, ContentType) => 
        val (file, contentType) = fc
        uploadToSonatype(file, contentType) 
    }) match {
      case Left(error) => 
        DefaultTaskResult(this, false, sw, message = Some(error))
      case Right(_) => 
        DefaultTaskResult(this, true, sw)
    }
  }

  private def uploadToSonatype(file : File, contentType : ContentType) : Either[ErrorMessage, String] = {
    val url = 
      "https://oss.sonatype.org/content/repositories/snapshots/" + 
        project.organization.replace('.', '/') + "/" + 
        project.artifactId + "/" + version + "/" + file.basename

    Post(url, new FileEntity(file, contentType))
  }

}
