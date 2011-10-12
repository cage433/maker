package starling.webservice

import starling.manager._
import org.mortbay.jetty.Server
import starling.props.Props
import javax.servlet.http.HttpServlet
import starling.utils.Log
import org.mortbay.jetty.servlet.{ServletHolder, Context}
import javax.servlet.Servlet

class RichServletHolder[S <: Servlet](val servlet: S, initParameters: (String, String)*) extends ServletHolder(servlet) {
  initParameters.foreach((setInitParameter _).tupled)
}

