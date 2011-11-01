package starling.loopyxl

import starling.auth.AuthHandler
import starling.manager._
import collection.JavaConversions
import starling.props.Props

class LoopyxlBromptonActivator extends BromptonActivator {

  def start(context: BromptonContext) = {
    val props = context.awaitService(classOf[Props])
    val auth = context.awaitService(classOf[AuthHandler])
    val methodsByService = new java.util.concurrent.ConcurrentHashMap[BromptonServiceReference,List[DynamicMethod]]()
    val methodsById = new java.util.concurrent.ConcurrentHashMap[Int,DynamicMethod]()
    context.createServiceTracker(None, ServiceProperties(ExportLoopyProperty), new BromptonServiceCallback[AnyRef] {
      def serviceAdded(ref: BromptonServiceReference, properties:ServiceProperties, service: AnyRef) = {
        val methods = new ReflectiveMethodSource(service).getMethods
        methods.foreach { method => methodsById.put(method.id, method) }
        methodsByService.put(ref, methods)
      }
     override def serviceRemoved(ref: BromptonServiceReference) = {
        val methods = methodsByService.get(ref)
        methodsByService.remove(ref)
        methods.foreach { method => methodsById.remove(method.id) }
      }
    })
    val osgiMethodSource = new MethodSource() {
      def getMethods = {
        import JavaConversions._
        methodsByService.values().foreach(println)
        methodsByService.values().flatten.toList
      }
      def lookup(methodId: Int) = {
        val method = methodsById.get(methodId)
        if (method == null) {
          throw new Exception("No method found for id " + methodId)
        }
        method
      }
    }

    val receiver = new LoopyXLReceiver(props.LoopyXLPort(), auth, osgiMethodSource)
    receiver.start

    context.onStopped { receiver.stop }
  }
}