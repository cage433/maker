package com.trafigura.services

import org.jboss.resteasy.plugins.server.servlet.ResteasyBootstrap
import javax.servlet.ServletContextEvent
import starling.utils.Log
import starling.utils.ImplicitConversions._
import javax.ws.rs.ext.RuntimeDelegate
import org.jboss.resteasy.spi.{ResteasyProviderFactory, Registry}


/* Instantiated by ScalaResteasyBootstrap
 */
trait WebServiceFactory {
  val serviceClasses: List[Class[_]] = Nil
  val services: List[AnyRef] = Nil
}

object ScalaResteasyBootstrap {
  RuntimeDelegate.setInstance(new ResteasyProviderFactory())
}

class ScalaResteasyBootstrap extends ResteasyBootstrap with Log {
  override def contextInitialized(event: ServletContextEvent) {
    val oldClassLoader = Thread.currentThread().getContextClassLoader()
    try {
      Thread.currentThread().setContextClassLoader(getClass.getClassLoader)
      super.contextInitialized(event)

      val servletContext = event.getServletContext

      val serviceFactoryClassName = servletContext.getInitParameter("traf.webservice.factory")

      if (serviceFactoryClassName != null) {
        val serviceFactory = Class.forName(serviceFactoryClassName).newInstance().cast[WebServiceFactory]
        val registry = servletContext.getAttribute(classOf[Registry].getName).cast[Registry]

        serviceFactory.serviceClasses.foreach(registry.addPerRequestResource(_))
        serviceFactory.services.foreach(registry.addSingletonResource(_))
      }
    } finally {
      Thread.currentThread().setContextClassLoader(oldClassLoader)
    }
  }

  override def contextDestroyed(event: ServletContextEvent) {
    super.contextDestroyed(event)
  }
}