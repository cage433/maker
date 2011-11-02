package starling.launcher

import org.mortbay.jetty.Server
import javax.servlet.http.HttpServlet
import org.mortbay.jetty.servlet.{ServletHolder, Context}
import starling.manager._


class  JettyBromptonActivator extends BromptonActivator {
  def start(context:BromptonContext) {
    val port = 7777
    val server = new Server(port)
    val rootContext = new Context(server, "/", Context.SESSIONS);

    context.createServiceTracker(Some( classOf[HttpServlet]), ServiceProperties(), new BromptonServiceCallback[HttpServlet]() {
      def serviceAdded(ref:BromptonServiceReference, properties:ServiceProperties, service:HttpServlet) {
        val servletPath = properties.get[HttpContext].context
        rootContext.addServlet(new ServletHolder(service.asInstanceOf[HttpServlet]), "/" + servletPath + "/*")
      }
    })

    try {
      server.start()
    } catch {
      case e:java.net.BindException => println("Not starting gui jetty as port " + port + " is already in use")
    }
  }
}