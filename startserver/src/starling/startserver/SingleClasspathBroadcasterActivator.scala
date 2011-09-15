package starling.startserver

import starling.utils.{Broadcaster, Receiver, ReceiversBroadcaster}
import starling.manager._

/**
 * Adds the things which usally get added in osgi
 */
class NullProps
class SingleClasspathBroadcasterActivator extends BromptonActivator {
  type Props = NullProps
  def defaults = new NullProps

  def start(context: BromptonContext) {
    val broadcaster = new ReceiversBroadcaster()
    context.createServiceTracker(Some(classOf[Receiver]), Nil, new BromptonServiceTracker {
      def serviceAdded(ref: BromptonServiceReference, service: AnyRef) {
        val receiver = service.asInstanceOf[Receiver]
        broadcaster.addReceiver(ref, receiver)
      }
      def serviceRemoved(ref: BromptonServiceReference) {
        broadcaster.removeReceiver(ref)
      }
    })
    context.registerService(classOf[Broadcaster], broadcaster)
  }

  def init(context: BromptonContext, props: NullProps) {}
  def stop(context: BromptonContext) {}
}