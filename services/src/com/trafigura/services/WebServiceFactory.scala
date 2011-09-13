package com.trafigura.services

import org.jboss.resteasy.plugins.server.servlet.ResteasyBootstrap
import org.jboss.resteasy.spi.Registry
import javax.servlet.ServletContextEvent
import starling.utils.Log
import starling.utils.ImplicitConversions._


/* Instantiated by ScalaResteasyBootstrap
 */
trait WebServiceFactory {
  val serviceClasses: List[Class[_]] = Nil
  val services: List[AnyRef] = Nil
}

class ScalaResteasyBootstrap extends ResteasyBootstrap with Log {
  override def contextInitialized(event: ServletContextEvent) {
    super.contextInitialized(event)

    val servletContext = event.getServletContext

    val serviceFactoryClassName = servletContext.getInitParameter("traf.webservice.factory")

    if (serviceFactoryClassName != null) {
      val serviceFactory = Class.forName(serviceFactoryClassName).newInstance().cast[WebServiceFactory]
      val registry = servletContext.getAttribute(classOf[Registry].getName).cast[Registry]

      serviceFactory.serviceClasses.foreach(registry.addPerRequestResource(_))
      serviceFactory.services.foreach(registry.addSingletonResource(_))
    }
  }

  override def contextDestroyed(event: ServletContextEvent) {
    super.contextDestroyed(event)
  }
}