package starling.startserver

import starling.utils.{Broadcaster, Receiver, ReceiversBroadcaster}
import starling.manager._

class SingleClasspathBroadcasterActivator extends BromptonActivator {

  def start(context: BromptonContext) {
    val broadcaster = new ReceiversBroadcaster()
    context.createServiceTracker(Some(classOf[Receiver]), ServiceProperties(), new BromptonServiceCallback[Receiver] {
      def serviceAdded(ref: BromptonServiceReference, properties:ServiceProperties, receiver: Receiver) {
        broadcaster.addReceiver(ref, receiver)
      }
      override def serviceRemoved(ref: BromptonServiceReference) {
        broadcaster.removeReceiver(ref)
      }
    })
    context.registerService(classOf[Broadcaster], broadcaster)
  }
}