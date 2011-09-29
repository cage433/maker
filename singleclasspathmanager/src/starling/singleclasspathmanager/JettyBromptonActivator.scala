package starling.singleclasspathmanager

import starling.manager.{BromptonServiceReference, BromptonServiceCallback, BromptonContext, BromptonActivator}
import javax.servlet.http.HttpServlet
import org.mortbay.jetty.servlet.{Context, ServletHolder}
import org.mortbay.jetty.{Server => JettyServer}

class JettyBromptonActivator extends BromptonActivator {
  def start(context:BromptonContext) {
//    val server = new JettyServer(7777)
//    val rootContext = new Context(server, "/", Context.SESSIONS);
//
//    context.createServiceTracker(Some( classOf[HttpServlet]), Nil, new BromptonServiceCallback[HttpServlet]() {
//      def serviceAdded(ref:BromptonServiceReference, service:HttpServlet) {
//        rootContext.addServlet(new ServletHolder(service.asInstanceOf[HttpServlet]), "/gotoValuationScreen/*")
//      }
//      def serviceRemoved(ref:BromptonServiceReference) {}
//    })
//
//    server.start()
  }

  def stop(context:BromptonContext) {}
}