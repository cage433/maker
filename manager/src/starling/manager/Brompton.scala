package starling.manager

import net.sf.cglib.proxy.{MethodProxy, MethodInterceptor, Enhancer}
import java.lang.reflect.Method
import java.io.{File, FileInputStream}
import java.util.{Dictionary, Properties}

class Brompton

trait BromptonActivator {
  type Props
  def defaults:Props
  def init(context:BromptonContext, props:Props)
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
  def completed() {
    val unused = lowercaseLookup.filterKeys(usedProperties.contains)
    if (unused.nonEmpty) {
      println("Unused properties:")
      unused.foreach(p => println("  " + p))
    }
  }
}


