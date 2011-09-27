package starling.manager

class Brompton

trait BromptonActivator {
  def start(context:BromptonContext)
  def stop(context:BromptonContext)
}

class ServiceProperty(val name:String, val value:AnyRef)

case class ServiceName(serviceName:String) extends ServiceProperty("name", serviceName)
case class HttpContext(context:String) extends ServiceProperty("alias", "/"+context)
object ExportGuiRMIProperty extends ServiceProperty("export.rmi.gui", java.lang.Boolean.TRUE)
object ExportTitanRMIProperty extends ServiceProperty("export.rmi.titan", java.lang.Boolean.TRUE)
object ExportExcelProperty extends ServiceProperty("export.excel", java.lang.Boolean.TRUE)

class NoServiceFoundException(message:String) extends RuntimeException(message)
case class BromptonServiceReference(id:String, klasses:List[String])

trait BromptonServiceCallback[T] {
  def serviceAdded(ref:BromptonServiceReference, service:T)
  def serviceRemoved(ref:BromptonServiceReference)
}

trait BromptonServiceTracker[T] {
  def each(f:T=>Unit):Unit
  def flatMap[R](f:T=>Iterable[R]) = {
    val arrayBuffer = new scala.collection.mutable.ArrayBuffer[R]()
    each { service => arrayBuffer ++= f(service) }
    arrayBuffer.toList
  }
}

trait BromptonServiceRegistration {
  def unregister():Unit
}

trait BromptonContext {
  def registerService[T](
      klass:Class[T],
      service:T,
      properties:List[ServiceProperty]=List()):BromptonServiceRegistration
  def awaitService[T](klass:Class[T]):T
  def createServiceTracker[T](
    klass:Option[Class[T]],
    properties:List[ServiceProperty]=Nil,
    serviceTracker:BromptonServiceCallback[T]=new BromptonServiceCallback[T] {
      def serviceAdded(ref: BromptonServiceReference, service: T) {}
      def serviceRemoved(ref: BromptonServiceReference) {}
    }):BromptonServiceTracker[T]
}

