package sproxy

import java.io._
import java.util._

import org.mortbay.jetty.Server
import org.mortbay.jetty.servlet.{ServletHolder, Context}
import org.mortbay.util.IO

import javax.servlet.http._
/**
 * This is a simple test webserer which has two test pages:
 *  -headers which lists the client headers
 *  -form/page which has a form which makes a post which then redirects
 */
object TestServer {
  def main(args:Array[String]) = {
    var server = new Server(9999)
    var root = new Context(server, "/", 0)
    root.addServlet(new ServletHolder(new RootServlet()), "/")
    root.addServlet(new ServletHolder(new HeadersServlet()), "/headers")
    root.addServlet(new ServletHolder(new PostServlet()), "/form/*")
    server.addHandler(root)
    server.start
  }
}
class RootServlet extends HttpServlet {
  override def doGet(request:HttpServletRequest, response:HttpServletResponse) {
    if (!request.getRequestURI.equals("/")) {
      response.setStatus(404)
      response.getWriter.println("404 expected /")
    } else {
      response.getWriter.println(
      <html><head></head><body>
        <h1>Test webserver</h1>
        <a href="headers">view client headers</a> <br/>
        <a href="form/page">test form with post</a> <br/>
      </body></html>
      )
    }
  }  
}
class HeadersServlet() extends HttpServlet {
  override def doGet(request:HttpServletRequest, response:HttpServletResponse) {
    response.getWriter.println("Request headers...")
    val headers = request.getHeaderNames
    while (headers.hasMoreElements) {
      val header = headers.nextElement
      val value = request.getHeader(header.asInstanceOf[String])
      response.getWriter.println( header + ": " + value )
    }
  }
 }
class PostServlet extends HttpServlet {
  var value = "abc"
  override def doGet(request:HttpServletRequest, response:HttpServletResponse) {
    if (!request.getRequestURI.equals("/form/page")) {
      response.setStatus(404)
      response.getWriter.println("404 did not expect " + request.getRequestURI)
    } else {
      val page = 
        <html><body>
        <p>{value}</p>
        <form action="formpost" method="post">
         <input type="text" name="formfield" value={value}/>
         <input type="submit" value="post"/>
        </form></body></html>
      response.getWriter.println( page )
    }
  }  
  override def doPost(request:HttpServletRequest, response:HttpServletResponse) {
    if (!request.getRequestURI.equals("/form/formpost")) {
      response.setStatus(404)
      response.getWriter.println("404 did not expect " + request.getRequestURI)
    } else {
      value = request.getParameter("formfield")
      response.sendRedirect("page")
    }
  }
}
