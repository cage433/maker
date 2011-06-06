package starling.http

import java.io.{StringWriter, PrintWriter, Writer}
import java.lang.String
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import javax.servlet.Servlet
import org.mortbay.jetty.handler.{ErrorHandler, AbstractHandler}
import org.mortbay.jetty.Request
import org.mortbay.jetty.servlet.{HashSessionManager, Context, ServletHolder}
import org.mortbay.jetty.webapp.WebAppContext
import org.mortbay.jetty.{Server => JettyServer}
import org.springframework.mail.javamail.JavaMailSenderImpl
import starling.props.Props
import org.mortbay.jetty.security._
import starling.props.Props
import xml._
import starling.utils.Log


class HttpServer(props:Props, servlets: (String, Servlet)*) {
  val server = new JettyServer(props.HttpPort())
  var servletPaths = List[String]()

  val rootContext = new Context(server, "/", Context.SESSIONS);

  val externalURL = props.ExternalUrl()

  def stop {
    server.stop
    server.join
  }

  // some test servlets
  registerServlet(new StatusServlet(), "status")
  registerServlet(new RestfulServlet("Some Test String"), "test")
  registerServlet(new org.jminix.console.servlet.MiniConsoleServlet(), "jmx")

  for((path, servlet) <- servlets) {
    val className : String = servlet.getClass.getName
    registerServlet(servlet, path)
  }

  def errorHandler = new ErrorHandler {
    override def handleErrorPage(request:HttpServletRequest, writer:Writer, code:Int, message:String) = {
      val xml = new WebPage(request) {
        val (_title, _body) = code match {
            case 404 => ("Page not found", <div>Page not found: {request.getRequestURI} <br/>
                        These are the valid root pages { rootPagesAsHtml } </div>)
            case 401 => ("Authentication error", <div>You need to log in with your windows username and password.</div>)
            case 500 => ("Server Error: " + message, stackTrace(request))
            case _ => (code + " " + (if (message==null) "" else message), NodeSeq.Empty)
          }
        def title = _title
        def body() = _body
      }.page()
      XML.write(writer, xml, "UTF-8", true, null)
    }
    private def stackTrace(request:HttpServletRequest):NodeSeq = {
      var t = request.getAttribute("javax.servlet.error.exception").asInstanceOf[Throwable]
      var exceptionHtmlList = List[Seq[Node]]()
      while (t != null) {
       val node = (
         <h3>Caused by:</h3>
         <pre> {
           val sw = new StringWriter()
           t.printStackTrace(new PrintWriter(sw))
           t = t.getCause
           sw.getBuffer().toString()
        }</pre>)
        exceptionHtmlList = (node :: exceptionHtmlList).reverse
      }
      exceptionHtmlList.flatMap(s=>s)
    }
  }

  def rootPagesAsHtml = {
    <ul> {  servletPaths.map{ path=>{
      val href = path match {
        case "jmx" => externalURL + "/jmx/"  //The jmx pages do not work though sproxy
        case _ => path+"/"
      }
      <li><a href={href}>{path}</a></li> ++ Unparsed("\n")
    }} } </ul>
  }

  def registerServlet(servlet: Servlet, path: String) {
    rootContext.addServlet(new ServletHolder(servlet), "/" + path + "/*")
    servletPaths ::= path
  }

  def run() = {
    // this needs to be the last servlet registered as it lists all the others
    registerServlet(new RootServlet(servletPaths), "")

    rootContext.setErrorHandler(errorHandler)
    server.addHandler(rootContext)

    server.start()

    Log.info("HttpServer stared on port: " + props.HttpPort())
  }

  class RootServlet(servletPaths : List[String]) extends HttpServlet {
    override def doGet(request: HttpServletRequest, response: HttpServletResponse) = {
      if (request.getRequestURI == "/") {
        val name = "starling - " + props.ServerName()
        response.setStatus(200)
        response.getWriter.println("<html><title>"+name+"</title>")
        response.getWriter.println("<html><body><ul>")
        response.getWriter.println("<h1>"+name+"</h1>")
        response.getWriter.println(rootPagesAsHtml)
        response.getWriter.println("</ul></body></html>")
      } else {
        response.setStatus(404)
      }
    }
  }
}

class RedirectServlet(to:String) extends HttpServlet {
  override def doGet(request: HttpServletRequest, response: HttpServletResponse) = {
    if (request.getRequestURI == "/") {
      response.sendRedirect(to)
    } else {
      response.setStatus(404)      
    }
  }
}

protected class Handler extends AbstractHandler {
  override def handle(target: String, request: HttpServletRequest, response: HttpServletResponse, dispatch: Int) = {
    response.setContentType("text/html");
    response.setStatus(HttpServletResponse.SC_OK);
    response.getWriter().println("hi");
    (request.asInstanceOf[Request]).setHandled(true);
  }
}
