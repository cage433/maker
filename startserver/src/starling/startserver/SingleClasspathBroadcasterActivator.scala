package starling.startserver

import starling.utils.{Broadcaster, Receiver, ReceiversBroadcaster}
import starling.manager._

class SingleClasspathBroadcasterActivator extends BromptonActivator {

  def start(context: BromptonContext) {
    val broadcaster = new ReceiversBroadcaster()
    context.createServiceTracker(Some(classOf[Receiver]), Nil, new BromptonServiceCallback[Receiver] {
      def serviceAdded(ref: BromptonServiceReference, receiver: Receiver) {
        broadcaster.addReceiver(ref, receiver)
      }
      def serviceRemoved(ref: BromptonServiceReference) {
        broadcaster.removeReceiver(ref)
      }
    })
    context.registerService(classOf[Broadcaster], broadcaster)
  }

  def stop(context: BromptonContext) {}
}