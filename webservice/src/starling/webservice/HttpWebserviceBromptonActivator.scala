package starling.webservice

import starling.manager._
import org.mortbay.jetty.Server
import starling.props.Props
import javax.servlet.http.HttpServlet
import starling.utils.Log
import org.mortbay.jetty.servlet.{ServletHolder, Context}


class HttpWebserviceBromptonActivator extends BromptonActivator with Log {

  def start(context: BromptonContext) = {
    val props = context.awaitService(classOf[Props])

    val httpPort = props.HttpPort()
    val server = new Server(httpPort)

    val rootContext = new Context(server, "/", Context.SESSIONS);
    val rootServlet = new RootServlet(props.ServerName())

    addRootServlet(rootContext, rootServlet, context)
    addWebServiceServlet(rootContext, rootServlet, context)

    log.info("Starting HTTP: " + httpPort)
    server.start()
  }

  private def addRootServlet(rootContext: Context, rootServlet: RootServlet, context: BromptonContext) {
    context.createServiceTracker(Some(classOf[HttpServlet]), ServiceProperties(), new BromptonServiceCallback[HttpServlet]() {
      def serviceAdded(ref: BromptonServiceReference, properties: ServiceProperties, service: HttpServlet) {
        val servletPath = properties.get[HttpContext].context

        rootContext.addServlet(new ServletHolder(service.asInstanceOf[HttpServlet]), "/" + servletPath + "/*")
        rootServlet.paths += servletPath
      }
    })

    rootContext.addServlet(new ServletHolder(rootServlet), rootServlet.path)
  }

  private def addWebServiceServlet(rootContext: Context, rootServlet: RootServlet, context: BromptonContext) {
    rootContext.addServlet(new RichServletHolder(new WebServiceServlet(context, rootServlet),
      "resteasy.injector.factory" → className[ScalaInjectorFactory],
      "resteasy.providers" → List(
        className[JsonSerializerMessageBodyWriter],
        className[JsonDeserializerMessageBodyReader],
        className[XmlSerializerMessageBodyWriter]
      ).mkString(", "),
      "resteasy.servlet.mapping.prefix" → "/RPC",
      "log4jConfigLocation" → "/utils/resources/log4j.properties"), "/RPC/*")
  }

  private def className[T: Manifest] = implicitly[Manifest[T]].erasure.getName
}