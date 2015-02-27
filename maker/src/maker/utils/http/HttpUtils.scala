package maker.utils.http

import maker.ConfigPimps
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.{HttpHost, HttpResponse, HttpStatus}
import org.apache.http.client.HttpClient
import org.scalatest.Failed
import org.apache.http.client.methods.HttpGet
import java.io.{File, FileOutputStream}
import maker.utils.{Int, FileUtils}
import maker.utils.FileUtils._
import com.typesafe.config.{ConfigFactory, Config}

class HttpUtils(config : Config = ConfigFactory.load()) extends ConfigPimps{

  import HttpUtils.{StatusCode, ErrorMessage}

  def downloadFile(url : String, file : File) : Either[(StatusCode, ErrorMessage), Unit] = {
    Get(url){
      response => 
        Option(response.getEntity()) match {
          case None => 
            // Don't know the circumstances that could cause a null response,
            // I couldn't reproduce it
            Left((-1, s"$url returned null entity"))
          case Some(entity) => 
            FileUtils.mkdirs(file.dirname)
            val fos = new FileOutputStream(file)
            entity.writeTo(fos)
            fos.close
            Right(Unit)
        }
    } 
  }

  def Get[U](path: String)(body: HttpResponse => U) : Either[(StatusCode, ErrorMessage), U] = {
    val req = new HttpGet(path)
    config.httpHeaders.foreach{
      case (field, value) => 
        req.addHeader(field, value)
    }

    withHttpClient{ client =>
      val response = client.execute(req)
      val statusCode = response.getStatusLine.getStatusCode
      if(statusCode == HttpStatus.SC_OK) 
        Right(body(response))
      else
        Left((statusCode, s"Failed to retrieve data from $path: ${response.getStatusLine}"))
    }
  }

  private def withHttpClient[U](body: HttpClient => U) : U = {

    val client = new DefaultHttpClient()
    config.proxy.foreach{
      case (host, port) => 
        val proxy = new HttpHost(host, port, "http")
    }
    try {
      body(client)
    }
    finally
      client.getConnectionManager.shutdown()
  }
}

object HttpUtils{
  type StatusCode =Int
  type ErrorMessage = String
}
