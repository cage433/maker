package starling.manager

import java.util.{Hashtable, Dictionary}
import swing.event.Event

class Brompton

trait Broadcaster {
  @DoNotCache def broadcast(event: Event)
}

trait Receiver {
  @DoNotCache def event(event: Event)
}

object Broadcaster {
  object Null extends Broadcaster { def broadcast(event: Event) {} }
}


trait BromptonActivator {
  def start(context:BromptonContext)
}

class ServiceProperty(val name:String, val value:AnyRef)

case class ServiceProperties(properties: ServiceProperty*) {
  def get[T <: ServiceProperty : Manifest]: T = {
    val propertyType = implicitly[Manifest[T]].erasure

    properties.find(property => propertyType.isAssignableFrom(property.getClass)).getOrElse(
      throw new Exception("No property of type: " + propertyType)).asInstanceOf[T]
  }

  def toDictionary: Dictionary[_, _] = mapToDictionary(properties.map(p => p.name → p.value).toMap)
  def toFilters: List[String] = properties.map { sp => "("+sp.name+"="+sp.value+")"}.toList
  def contains(serviceProperties: ServiceProperties) = serviceProperties.properties.forall(p => properties.contains(p))

  private def mapToDictionary(map:Map[String,AnyRef]):Dictionary[_,_] = {
    val table = new Hashtable[String,AnyRef]()
    map.foreach ( kv => table.put(kv._1, kv._2))
    table
  }
}


case class ServiceName(serviceName:String) extends ServiceProperty("name", serviceName)
case class HttpContext(context:String) extends ServiceProperty("alias", "/"+context)
object ExportGuiRMIProperty extends ServiceProperty("export.rmi.gui", java.lang.Boolean.TRUE)
object ExportTitanRMIProperty extends ServiceProperty("export.rmi.titan", java.lang.Boolean.TRUE)
object ExportTitanHTTPProperty extends ServiceProperty("export.http.titan", java.lang.Boolean.TRUE)
object ExportXlloopProperty extends ServiceProperty("export.xlloop", java.lang.Boolean.TRUE)
object ExportLoopyProperty extends ServiceProperty("export.loopy", java.lang.Boolean.TRUE)


class NoServiceFoundException(message:String) extends RuntimeException(message)
case class BromptonServiceReference(id:String, klasses:List[String])

trait BromptonServiceCallback[T] {
  def serviceAdded(ref:BromptonServiceReference, properties:ServiceProperties, service:T)
  def serviceRemoved(ref:BromptonServiceReference) {}
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
  def onStarted(action: => Unit)
  def onStopped(action: => Unit)

  def registerService[T](
      klass:Class[T],
      service:T,
      properties:ServiceProperties=ServiceProperties()):BromptonServiceRegistration
  def awaitService[T](klass:Class[T]):T
  def createServiceTracker[T](
    klass:Option[Class[T]],
    properties:ServiceProperties=ServiceProperties(),
    tracker:BromptonServiceCallback[T]=new BromptonServiceCallback[T] {
      def serviceAdded(ref: BromptonServiceReference, properties:ServiceProperties, service: T) {}
    }):BromptonServiceTracker[T]
}

