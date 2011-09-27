package starling.manager

import net.sf.cglib.proxy.{MethodProxy, MethodInterceptor, Enhancer}
import java.lang.reflect.Method
import java.io.{File, FileInputStream}
import java.util.{Dictionary, Properties}


// TODO [DM][2011-09-27] unused?
class Brompton

/**
 * BromptonActivator provides a contract for initialising, starting and stopping a service that may wish to register
 * itself with a container/context) and that requires properties.
 *
 * @diagram dev/services/starling/docs/Server Activation.png
 * @see BromptonContext
 * @documented
 */
trait BromptonActivator {
  type Props
  def defaults:Props
  def init(context:BromptonContext, props:Props)
  def start(context:BromptonContext)
  def stop(context:BromptonContext)
}

/**
 * A generic (name, value) property pair for a service.
 * 
 * @documented
 */
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

/**
 * BromptonServiceTracker provides a means of generating callbacks for each registered service of a certain type, or of
 * dynamically mapping each to a different type.
 * @documented
 */
trait BromptonServiceTracker[T] {
  def each(f:T=>Unit):Unit
  def flatMap[R](f:T=>Iterable[R]) = {
    val arrayBuffer = new scala.collection.mutable.ArrayBuffer[R]()
    each { service => arrayBuffer ++= f(service) }
    arrayBuffer.toList
  }
}

/**
 * BromptonServiceRegistration provides the means to unregister a previously registered service.
 * @documented
 */
trait BromptonServiceRegistration {
  def unregister():Unit
}

/**
 * A BromptonServiceContext provides a contract to allow the generic registration, tracking and retrieval of a
 * service by its registered type.
 *
 * @see BromptonServiceRegistration
 * @see BromptonServiceCallback
 * @diagram dev/services/starling/docs/Server Activation.png
 * @documented
 */
trait BromptonContext {
  /**
   * Registers the given service against its type, with an optional list of properties.
   * @param klass The type under which the service should be registered, must be assignable from the service's type.
   * @param The service, may not be null.
   * @param properties The service's properties, optional.
   * @return A instance which may be used to unregister the service.
   */
  def registerService[T](
      klass:Class[T],
      service:T,
      properties:List[ServiceProperty]=List()):BromptonServiceRegistration

  /**
   * @return The service of the given type registered in this context.
   * @throws Exception may be thrown if exactly one service of the given type is not held in this context.
   */
  def awaitService[T](klass:Class[T]):T

  /**
   * Creates then returns a service tracker.
   * @param klass The type of service to track, may not be null.
   * @param properties An optional list of properties.
   * @param serviceTracker The callback listener.
   * @return The new tracker.
   */
  def createServiceTracker[T](
    klass:Option[Class[T]],
    properties:List[ServiceProperty]=Nil,
    serviceTracker:BromptonServiceCallback[T]=new BromptonServiceCallback[T] {
      def serviceAdded(ref: BromptonServiceReference, service: T) {}
      def serviceRemoved(ref: BromptonServiceReference) {}
    }):BromptonServiceTracker[T]
}

/**
 * The Props object provides a singleton helper to read then return the contents of the application's "props.conf".
 * @diagram dev/services/starling/docs/Server Activation.png
 * @documented
 */
object Props {
  def readDefault = {
    val p = new Properties()
    List(new File("props.conf")).foreach { file =>
      if (file.exists) {
        p.load(new FileInputStream(file))
      }
    }
    dictionaryToMap[String,String](p)
  }
  private def dictionaryToMap[K,V](dictionary:Dictionary[_,_]):Map[K,V] = {
    var map = Map[K,V]()
    val enumeration = dictionary.keys
    while (enumeration.hasMoreElements) {
      val key = enumeration.nextElement
      val value = dictionary.get(key)
      map = map.updated(key.asInstanceOf[K], value.asInstanceOf[V])
    }
    map
  }
}

/**
 * Props provides the implementation for a custom properties map.  It uses bytecode manipulation to allow matching
 * method name properties to override actual method calls, for a given class loader's types.
 * @diagram dev/services/starling/docs/Server Activation.png
 * @documented
 */
class Props(inputProperties:Map[String,String]) {
  val normalised = inputProperties.map { case (k,v) => k.toLowerCase -> v }
  val lowercaseLookup = inputProperties.map { case (k,_) => k.toLowerCase -> k }
  val stringClass = classOf[String]
  val intClass = classOf[Int]
  val booleanClass = classOf[Boolean]
  val usedProperties = new scala.collection.mutable.HashSet[String]()
  def applyOverrides[T](classLoader:ClassLoader, props:T):T = {
    val e = new Enhancer()
    e.setClassLoader(classLoader)//classOf[Props].getClassLoader)
    e.setSuperclass(props.asInstanceOf[AnyRef].getClass)
    e.setCallback(new MethodInterceptor() {
       def intercept(obj:Object, method:Method,
                    args:Array[Object], proxy:MethodProxy):Object = {
         val lowerName = method.getName.toLowerCase
         normalised.get(lowerName) match {
           case Some(value) => {
             usedProperties += lowerName
             method.getReturnType match {
               case `stringClass` => value
               case `intClass` => new java.lang.Integer(value)
               case `booleanClass` => java.lang.Boolean.valueOf(value)
               case other => throw new Exception("Unknown  type " + other)
             }
           }
           case None => proxy.invokeSuper(obj, args)
         }
       }
    })
    e.create().asInstanceOf[T]
  }

  /**
   * Informs this instance that its lifecycle is over.  It prints a list of unused properties to the standard output
   * stream.
   */
  def completed() {
    val unused = lowercaseLookup.filterKeys(usedProperties.contains)
    if (unused.nonEmpty) {
      println("Unused properties:")
      unused.foreach(p => println("  " + p))
    }
  }
}
