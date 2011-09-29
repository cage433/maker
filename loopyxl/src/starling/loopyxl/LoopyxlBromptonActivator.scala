package starling.loopyxl

import starling.auth.AuthHandler
import starling.manager._
import collection.JavaConversions
import starling.props.Props

class LoopyxlBromptonActivator extends BromptonActivator {

  var receiver : LoopyXLReceiver = _

  def start(context: BromptonContext) = {
    val props = context.awaitService(classOf[Props])
    val auth = context.awaitService(classOf[AuthHandler])
    val methodsByService = new java.util.concurrent.ConcurrentHashMap[BromptonServiceReference,List[DynamicMethod]]()
    val methodsById = new java.util.concurrent.ConcurrentHashMap[Int,DynamicMethod]()
    context.createServiceTracker(None,  ExportExcelProperty::Nil, new BromptonServiceCallback[AnyRef] {
      def serviceAdded(ref: BromptonServiceReference, service: AnyRef) = {
        val methods = new ReflectiveMethodSource(service).getMethods
        methods.foreach { method => methodsById.put(method.id, method) }
        methodsByService.put(ref, methods)
        println("Adding methods " + ref + " " + methods.map(_.name))
      }
      def serviceRemoved(ref: BromptonServiceReference) = {
        val methods = methodsByService.get(ref)
        methodsByService.remove(ref)
        methods.foreach { method => methodsById.remove(method.id) }
      }
    })
    val osgiMethodSource = new MethodSource() {
      def getMethods = {
        import JavaConversions._
        methodsByService.values().foreach(println)
        println("getmethods " + methodsByService.values().flatten.toList.map(_.name))
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
    receiver = new LoopyXLReceiver(props.LoopyXLPort(), auth, osgiMethodSource)
    receiver.start
  }

  def stop(context: BromptonContext) = {
    if (receiver != null) receiver.stop
  }
}