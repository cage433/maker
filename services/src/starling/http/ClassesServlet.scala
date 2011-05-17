package starling.http

import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import org.mortbay.util.IO
import starling.props.PropsHelper
import java.io.OutputStreamWriter
import starling.props.PropsHelper

/**
 * Serves up the classpath over http
 */
class ClassesServlet(prefix:String) extends HttpServlet {

  override def doGet(request: HttpServletRequest, response: HttpServletResponse): Unit = {
    val resourceWithSlash = request.getRequestURI.substring(prefix.length + 1)
    val resource = if (resourceWithSlash.isEmpty) "" else resourceWithSlash.substring(1)
    val outputStream = response.getOutputStream
    try {
      val stream = classOf[ClassesServlet].getClassLoader.getResourceAsStream(resource)
      if (stream == null) {
        response.sendError(404, "Resource not found: " + resource)
      } else {
        IO.copy(stream, outputStream)
      }
    } catch {
      case e:Exception => {
        e.printStackTrace()
        response.sendError(500, "Error: " + e)
      }
    }
  }
}