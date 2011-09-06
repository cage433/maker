package starling.singleclasspathmanager

import starling.manager.{BromptonServiceReference, BromptonServiceTracker, BromptonContext, BromptonActivator}
import javax.servlet.http.HttpServlet
import org.mortbay.jetty.servlet.{Context, ServletHolder}
import org.mortbay.jetty.{Server => JettyServer}

class JettyBromptonActivatorProps
class JettyBromptonActivator extends BromptonActivator {
  type Props = JettyBromptonActivatorProps
  def defaults = new JettyBromptonActivatorProps
  def init(context:BromptonContext, props:JettyBromptonActivatorProps) {}

  def start(context:BromptonContext) {
    val server = new JettyServer(7777)
    val rootContext = new Context(server, "/", Context.SESSIONS);

    context.createServiceTracker(Some( classOf[HttpServlet]), Nil, new BromptonServiceTracker() {
      def serviceAdded(ref:BromptonServiceReference, service:AnyRef) {
        rootContext.addServlet(new ServletHolder(service.asInstanceOf[HttpServlet]), "/gotoValuationScreen")
      }
      def serviceRemoved(ref:BromptonServiceReference) {}
    })

    server.start()
  }

  def stop(context:BromptonContext) {}
}