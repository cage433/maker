package starling.startserver

import starling.utils.{Broadcaster, Receiver, ReceiversBroadcaster}
import starling.manager._

/**
 * Adds the things which usually get added in osgi.
 */
class NullProps

/**
 * SingleClasspathBroadcasterActivator is an implementation to create then register a ReceiversBroadcaster service in
 * its start method.  There it also creates a tracker to enable added/removed services to be tracked.
 *
 * @documented
 */
class SingleClasspathBroadcasterActivator extends BromptonActivator {
  type Props = NullProps
  def defaults = new NullProps

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

  def init(context: BromptonContext, props: NullProps) {}
  def stop(context: BromptonContext) {}
}