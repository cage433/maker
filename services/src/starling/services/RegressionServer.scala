package starling.services

import org.mortbay.jetty.bio.{SocketConnector}
import org.mortbay.jetty.servlet.{ServletHolder, Context}
import org.mortbay.jetty.{Server => JettyServer}
import starling.http.ReportServlet

class RegressionServer(port: Int, servlet: ReportServlet) {
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

  def start {
    regressionServer.start
  }

  def stop {
    regressionServer.stop
  }
}