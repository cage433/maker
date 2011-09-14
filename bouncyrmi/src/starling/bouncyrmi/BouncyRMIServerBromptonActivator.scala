package starling.bouncyrmi

import starling.auth.{AuthHandler, User}
import org.jboss.netty.channel.{ChannelLocal, Channel}
import java.util.concurrent.Executors
import swing.event.Event
import starling.manager._
import starling.utils.{Receiver, Log, Broadcaster, ThreadNaming}

//UnrecognisedTradeIDException
class BouncyRMIServerProps {
  def rmiPort:Int = 15715
  def starlingServiceRmiPort:Int = 112234
}

class BouncyRMIServerBromptonActivator extends BromptonActivator {
  type Props = BouncyRMIServerProps

  def defaults = new BouncyRMIServerProps

  var rmiServerForGUI:BouncyRMIServer = _
  var rmiServerForTitan : BouncyRMIServer = _

  def init(context: BromptonContext, props: BouncyRMIServerProps) {
    val authHandler = context.awaitService(classOf[AuthHandler])
    val receiver = new Receiver {
      def event(event: Event) = {
        rmiServerForGUI.publish(event)
      }
    }
    context.registerService(classOf[Receiver], receiver)

    rmiServerForGUI = new BouncyRMIServer(
      props.rmiPort, authHandler, BouncyRMI.CodeVersionUndefined, Set("starling.gui.api.UnrecognisedTradeIDException")
    )

    Log.info("Initialize public services for Titan components, service port: " + props.starlingServiceRmiPort)

    rmiServerForTitan = new BouncyRMIServer(
      props.starlingServiceRmiPort,
      AuthHandler.Dev, BouncyRMI.CodeVersionUndefined,
      Set("com.trafigura.services.valuation.TradeManagementCacheNotReady", "java.lang.IllegalArgumentException")
    )

    def registerServiceTracker(serviceProperty:ServiceProperty, rmiServer:BouncyRMIServer) {
      context.createServiceTracker(None, serviceProperty::Nil, new BromptonServiceTracker {
        def serviceAdded(ref: BromptonServiceReference, service: AnyRef) = {
          rmiServer.addInstance(ref.klasses.head, service)
        }
        def serviceRemoved(ref: BromptonServiceReference) = {
          rmiServer.removeInstance(ref.klasses.head)
        }
      })
    }

    registerServiceTracker(ExportGuiRMIProperty, rmiServerForGUI)
    registerServiceTracker(ExportTitanRMIProperty, rmiServerForTitan)

    rmiServerForGUI.start
    rmiServerForTitan.start

  }

  def start(context: BromptonContext) {}

  def stop(context: BromptonContext) {}
}


//class RMIBroadcaster(rmiServer0: => BouncyRMIServer) extends Broadcaster {
//  lazy val executor = Executors.newCachedThreadPool()
//  lazy val rmiServer = rmiServer0
//
//  def broadcast(event: Event) = if (!event.isInstanceOf[RabbitEvent] && rmiServer != null) {
//    executor.execute { rmiServer.publish(EventBatch(List(event))) }
//  }
//}