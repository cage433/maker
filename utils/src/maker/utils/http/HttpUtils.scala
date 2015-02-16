package maker.utils.http

import maker.MakerConfig
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.{HttpHost, HttpResponse, HttpStatus}
import org.apache.http.client.HttpClient
import org.scalatest.Failed
import org.apache.http.client.methods.HttpGet

class HttpUtils extends MakerConfig{

  type ErrorMessage = String

  def Get[U](path: String, headers : (String, String)*)(body: HttpResponse => U) : Either[ErrorMessage, U] = {
    val req = new HttpGet(s"path")
    headers.foreach{
      case (key, value) => 
        req.addHeader(key,value)
    }

    withHttpClient{ client =>
      val response = client.execute(req)
      if(response.getStatusLine.getStatusCode == HttpStatus.SC_OK) 
        Right(body(response))
      else
        Left(s"Failed to retrieve data from $path: ${response.getStatusLine}")
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
