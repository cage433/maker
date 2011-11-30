package starling.bouncyrmi

import starling.auth.{AuthHandler, User}
import org.jboss.netty.channel.{ChannelLocal, Channel}
import java.util.concurrent.Executors
import swing.event.Event
import starling.manager._
import starling.utils._

class BouncyRMIServerBromptonActivator extends BromptonActivator {

  var rmiServerForGUI:BouncyRMIServer = _
  var rmiServerForTitan : BouncyRMIServer = _

  private def isGui(eventClass:Class[_]) = {
    val allEventTypes = Reflection.allTypes(eventClass).flatMap { klass => {
      klass.getAnnotation(classOf[EventType]) match {
        case null => None
        case a => Some(a.value)
      }
    }}.toSet
    if (allEventTypes.size > 1) throw new Exception(eventClass + " defines more than one event type: " + allEventTypes)
    allEventTypes.contains("GUI")
  }

  def start(context: BromptonContext) {
    val props = context.awaitService(classOf[starling.props.Props])
    val authHandler = context.awaitService(classOf[AuthHandler])
    val codeMD5s = context.awaitService(classOf[String])

    val eventTypes = new java.util.concurrent.ConcurrentHashMap[Class[_],Boolean]()
    val receiver = new Receiver {
      def event(event: Event) = {
        val eventClass = event.getClass
        if (!eventTypes.contains(eventClass)) {
           eventTypes.put(eventClass, isGui(eventClass))
        }
        if (eventTypes.get(eventClass)) {
          rmiServerForGUI.publish(event)
        }
      }
    }
    context.registerService(classOf[Receiver], receiver)

    rmiServerForGUI = new BouncyRMIServer(
      props.RmiPort(), authHandler, codeMD5s, Set("starling.gui.api.UnrecognisedTradeIDException"), registerUserMBean = true
    )

    Log.info("Initialize public services for Titan components, service port: " + props.StarlingServiceRmiPort())

    rmiServerForTitan = new BouncyRMIServer(
      props.StarlingServiceRmiPort(),
      AuthHandler.Dev, BouncyRMI.CodeVersionUndefined,
      Set("com.trafigura.services.valuation.TradeManagementCacheNotReady", "java.lang.IllegalArgumentException")
    )

    def registerServiceTracker(serviceProperty:ServiceProperty, rmiServer:BouncyRMIServer) {
      context.createServiceTracker(None, ServiceProperties(serviceProperty), new BromptonServiceCallback[AnyRef] {
        def serviceAdded(ref: BromptonServiceReference, properties:ServiceProperties, service: AnyRef) = {
          rmiServer.addInstance(Class.forName(ref.klasses.head), service)
        }
        override def serviceRemoved(ref: BromptonServiceReference) = {
          rmiServer.removeInstance(Class.forName(ref.klasses.head))
        }
      })
    }

    registerServiceTracker(ExportGuiRMIProperty, rmiServerForGUI)
    registerServiceTracker(ExportTitanRMIProperty, rmiServerForTitan)

    rmiServerForGUI.start
    rmiServerForTitan.start

  }
}


//class RMIBroadcaster(rmiServer0: => BouncyRMIServer) extends Broadcaster {
//  lazy val executor = Executors.newCachedThreadPool()
//  lazy val rmiServer = rmiServer0
//
//  def broadcast(event: Event) = if (!event.isInstanceOf[RabbitEvent] && rmiServer != null) {
//    executor.execute { rmiServer.publish(EventBatch(List(event))) }
//  }
//}