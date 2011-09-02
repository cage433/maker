package starling.manager

import net.sf.cglib.proxy.{MethodProxy, MethodInterceptor, Enhancer}
import java.lang.reflect.Method
import scala.swing.Publisher
import java.util.Properties
import collection.JavaConversions
import java.io.{File, FileInputStream}

class Brompton

trait BromptonActivator {
  type Props
  def defaults:Props
  def init(context:BromptonContext, props:Props)
  def start(context:BromptonContext)
  def stop(context:BromptonContext)
}

class ServiceProperty(val name:String, val value:AnyRef)
//class ServiceRank(rank:Int) extends ServiceProperty(Constants.SERVICE_RANKING, new java.lang.Integer(rank))
case class ServiceName(serviceName:String) extends ServiceProperty("name", serviceName)
case class HttpContext(context:String) extends ServiceProperty("alias", "/"+context)
object ExportRMIProperty extends ServiceProperty("export.rmi", java.lang.Boolean.TRUE)
object ExportExcelProperty extends ServiceProperty("export.excel", java.lang.Boolean.TRUE)

class NoServiceFoundException(time:Long) extends RuntimeException("No service found after " + time + "s")
case class BromptonServiceReference(id:String)

trait BromptonServiceTracker {
  def serviceAdded(ref:BromptonServiceReference, service:AnyRef)
  def serviceRemoved(ref:BromptonServiceReference)
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
  def createServiceTracker(klass:Option[Class[_]], properties:List[ServiceProperty], serviceTracker:BromptonServiceTracker)
}

object Props {
  def readDefault = {
    val p = new Properties()
    List(new File("props.conf"), new File("generated.props.conf")).foreach { file =>
      if (file.exists) {
        p.load(new FileInputStream(file))
      }
    }
    p
  }
}

class Props(inputProperties:Map[String,String]) {
  val normalised = inputProperties.map { case (k,v) => k.toLowerCase -> v }
  val lowercaseLookup = inputProperties.map { case (k,_) => k.toLowerCase -> k }
  val stringClass = classOf[String]
  val intClass = classOf[Int]
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


