package starling.webservice

import starling.manager._
import org.mortbay.jetty.Server
import starling.props.Props
import javax.servlet.http.HttpServlet
import org.mortbay.jetty.servlet.{ServletHolder, Context}
import starling.utils.Log


class HttpWebserviceBromptonActivator extends BromptonActivator with Log {
  def start(context: BromptonContext) = {
    val props = context.awaitService(classOf[Props])

    val httpPort = props.HttpPort()
    val server = new Server(httpPort)

    val rootContext = new Context(server, "/", Context.SESSIONS);
    val rootServlet = new RootServlet(props.ServerName())
    rootContext.addServlet(new ServletHolder(rootServlet), rootServlet.path)

    context.createServiceTracker(Some( classOf[HttpServlet]), ServiceProperties(), new BromptonServiceCallback[HttpServlet]() {
      def serviceAdded(ref:BromptonServiceReference, properties:ServiceProperties, service:HttpServlet) {
        val servletPath = properties.get[HttpContext].context

        rootContext.addServlet(new ServletHolder(service.asInstanceOf[HttpServlet]), "/"+servletPath+"/*")
        rootServlet.paths += servletPath
      }
    })

    server.start()
    log.info("Starting HTTP: " + httpPort)
  }
}