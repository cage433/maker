package starling.services

import org.mortbay.jetty.bio.{SocketConnector}
import org.mortbay.jetty.servlet.{ServletHolder, Context}
import org.mortbay.jetty.{Server => JettyServer}
import starling.http.ReportServlet
import starling.utils.Stopable

class RegressionServer(port: Int, servlet: ReportServlet) extends Stopable {
  lazy val regressionServer = {
    val server = new JettyServer()
    val connector = new SocketConnector()
    connector.setHost("127.0.0.1")
    connector.setPort(port)
    server.addConnector(connector)
    val rootContext = new Context(server, "/", Context.SESSIONS);
    rootContext.addServlet(new ServletHolder(servlet), "/reports/*")
    server
  }

  override def start { super.start; regressionServer.start }
  override def stop  { super.stop;  regressionServer.stop  }
}