package starling.webservice

import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import collection.mutable.ListBuffer
import xml.Unparsed

case class RootServlet(serverName: String, path: String = "/", paths: ListBuffer[String] = ListBuffer())
  extends HttpServlet {

  val name = "starling - " + serverName

  override def doGet(request: HttpServletRequest, response: HttpServletResponse) = if (request.getRequestURI == path) {
    response.setStatus(200)
    response.getWriter.println("<html><title>"+name+"</title>")
    response.getWriter.println("<html><body><ul>")
    response.getWriter.println("<h1>"+name+"</h1>")
    response.getWriter.println(rootPagesAsHtml)
    response.getWriter.println("</ul></body></html>")
  } else {
    response.setStatus(404)
  }

  def rootPagesAsHtml = {
    <ul> {  paths.map{ path=>{
      val href = path match {
        case _ => path+"/"
      }
      <li><a href={href}>{path}</a></li> ++ Unparsed("\n")
    }} } </ul>
  }
}

