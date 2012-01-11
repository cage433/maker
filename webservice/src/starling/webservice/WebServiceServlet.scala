package starling.webservice

import meta.DocumentationService
import starling.manager._
import org.jboss.resteasy.spi.Registry
import javax.servlet.{ServletConfig, ServletContext}
import org.jboss.resteasy.plugins.server.servlet.HttpServletDispatcher
import scalaz.Scalaz._


class WebServiceServlet(context: BromptonContext, rootServlet: RootServlet) extends HttpServletDispatcher {
  override def init(servletConfig: ServletConfig) {
    super.init(servletConfig)

    Option(servletConfig.getServletContext).foreach(trackWebServices(_))
  }

  private def trackWebServices(servletContext: ServletContext) {
    val registry = servletContext.getAttribute(classOf[Registry].getName).asInstanceOf[Registry]

    val documentationService = new DocumentationService()
    registry.addSingletonResource(documentationService)
    addRootPath(classOf[DocumentationService])

    context.createServiceTracker(None, ServiceProperties(ExportTitanHTTPProperty), new BromptonServiceCallback[AnyRef] {
      def serviceAdded(ref: BromptonServiceReference, properties: ServiceProperties, service: AnyRef) = service match {
        case serviceClass:Class[_] => addPerRequestResource(serviceClass)
        case _ => addSingletonResource(service, properties.get[ProxiedService].fold(_.implementationClass, service.getClass))
      }
    })

    def addRootPath(serviceClass: Class[_]) = rootServlet.paths +=
      "RPC/Doc/Forms" + documentationService.serviceFor(serviceClass.getName).uri

    def addPerRequestResource(serviceClass: Class[_]) = {
      registry.addPerRequestResource(serviceClass)
      documentationService.registerClasses(serviceClass)
      addRootPath(serviceClass)
    }

    def addSingletonResource(service: AnyRef, serviceClass: Class[_]) = {
      registry.addSingletonResource(service);
      documentationService.registerClasses(serviceClass)
      addRootPath(serviceClass)
    }
  }
}