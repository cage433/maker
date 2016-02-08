package maker.task.tasks

import org.apache.http.auth.{AuthScope, UsernamePasswordCredentials}
import org.apache.http.impl.client._
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.methods.{HttpPost, HttpGet}
import org.apache.http.client.HttpClient
import org.apache.http.{HttpStatus, HttpResponse, HttpHost}
import org.apache.http.entity._
import org.apache.http.conn.params.ConnRoutePNames
import org.apache.http.util.EntityUtils
import java.io._
import maker.project.ProjectTrait

trait SonatypeTask {
  def project: ProjectTrait
  val Array(sonatypeUsername, sonatypePassword) = project.sonatypeCredentials
  val credentialHost = "oss.sonatype.org"
  type ErrorMessage = String
  //
  // TODO - add configurable proxy
  //val proxy = new HttpHost("127.0.0.1", 4128, "http")

  private def withHttpClient[U](body: HttpClient => U) : U = {

    val credsProvider = new BasicCredentialsProvider()
    credsProvider.setCredentials(
      new AuthScope(credentialHost, AuthScope.ANY_PORT),
      new UsernamePasswordCredentials(sonatypeUsername, sonatypePassword))
      val httpclient = HttpClients.custom()
            .setDefaultCredentialsProvider(credsProvider)
            .build()
    try {
      println("Posting")
      body(httpclient)
    }
    finally
      httpclient.close()
  }

  def Get[U](path:String)(body: HttpResponse => U) : U = {
    val req = new HttpGet(path)
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
    val req = new HttpPost(path)
    req.setEntity(entity)
    withHttpClient{ client =>
      val response = client.execute(req)
      if (response.getStatusLine.getStatusCode == HttpStatus.SC_CREATED)
        Right(EntityUtils.toString(response.getEntity))
      else 
        Left(s"${response}")
    }
  }
}
