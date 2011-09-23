package starling.startserver

import starling.utils.{Broadcaster, Receiver, ReceiversBroadcaster}
import starling.manager._

/**
 * Adds the things which usually get added in OSGi.
 *
 * @documented
 */
class NullProps

/**
 * SingleClasspathBroadcasterActivator is an implementation to create then register a ReceiversBroadcaster service in
 * its start method.  There it also creates a service tracker to enable any added/removed services to be similarly
 * chained to its broadcaster service.
 *
 * @documented
 */
class SingleClasspathBroadcasterActivator extends BromptonActivator {
  type Props = NullProps
  def defaults = new NullProps
  /**
   * Creates a ReceiversBroadcaster tracked to the given context then registers it with the context.
   *
   * @param context The context.
   */
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
  /**
   * Does nothing.
   */
  def init(context: BromptonContext, props: NullProps) {}
  /**
   * Does nothing.
   */
  def stop(context: BromptonContext) {}
}
