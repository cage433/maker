package starling.http

import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

class StatusServlet extends HttpServlet {
  override def doGet(request: HttpServletRequest, response: HttpServletResponse) = {
    response.setContentType("text/plain")
    val writer = response.getWriter()
    writer.append("OK")
  }
}