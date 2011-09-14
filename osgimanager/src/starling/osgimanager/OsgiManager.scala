package starling.osgimanager

import java.lang.reflect.{Method, InvocationHandler}
import starling.manager.BromptonServiceReference._
import org.osgi.framework.{BundleActivator => OSGIBundleActivator}
import org.osgi.framework.{BundleContext => OSGIBundleContext}
import org.osgi.framework.ServiceReference
import org.osgi.framework._
import java.util.concurrent.atomic.AtomicReference
import java.io.{FileInputStream, File}
import java.util.{Dictionary,Hashtable,Properties}
import java.lang.reflect.{Proxy,InvocationHandler,Method}
import org.osgi.framework.Bundle
import org.osgi.framework.BundleEvent
import org.osgi.service.cm.ManagedService
import org.osgi.service.cm.ConfigurationAdmin
import starling.manager._
import swing.Publisher
import org.osgi.util.tracker.{ServiceTrackerCustomizer, ServiceTracker => OSGIServiceTracker, BundleTracker, BundleTrackerCustomizer}
import java.util.concurrent.{CountDownLatch, ConcurrentHashMap}
import swing.event.Event
import starling.utils.{Receiver, Broadcaster, ReceiversBroadcaster}
import collection.JavaConversions

trait PropsService {
  def updated(props:Dictionary[String,String])
}

class OsgiManager

class BundleHandle(var holder:Option[BundleActivatorHolder])

class BromptonOSGIActivator extends OSGIBundleActivator {
  var bundleTracker:BundleTracker = _
  var props:Option[Props] = None
  def start(context:OSGIBundleContext) {
    val latch = context.getService(context.getServiceReference(classOf[CountDownLatch].getName)).asInstanceOf[CountDownLatch]
    import Bundle._

    bundleTracker = new BundleTracker(context, UNINSTALLED|INSTALLED|RESOLVED|STARTING|STOPPING|ACTIVE, new BundleTrackerCustomizer() {
      private def createHolder(bundle:Bundle) = {
        val activatorName = bundle.getHeaders.get("Brompton-Activator").asInstanceOf[String]
        if (activatorName != null) {
          val activatorClass:Class[Object] = bundle.loadClass(activatorName).asInstanceOf[Class[Object]]
          val activator = {
            val instance = activatorClass.newInstance
            instance.asInstanceOf[BromptonActivator]
          }
          val context = new OSGIBromptonContext(bundle.getBundleContext)
          val holder = new BundleActivatorHolder(activator, context, latch)
          println(">>Starting: " + bundle.getSymbolicName)
          holder.doStart(props.get)
          Some(holder)
        } else {
          None
        }
      }
      def addingBundle(bundle:Bundle, event:BundleEvent):Object = {
        val holder = if (bundle.getState==Bundle.ACTIVE) createHolder(bundle) else None
        new BundleHandle(holder)
      }
      def modifiedBundle(bundle:Bundle, event:BundleEvent,obj:Object) {
        val handle = obj.asInstanceOf[BundleHandle]
        event.getType match {
          case BundleEvent.STARTED => {
            handle.holder = createHolder(bundle)
          }
          case BundleEvent.STOPPING => {
            handle.holder match {
              case None => //Nothing
              case Some(holder) => {
                println(">>Stoping: " + bundle.getSymbolicName)
                holder.stop
                handle.holder = None
              }
            }
          }
          case _ =>
        }
      }
      def removedBundle(bundle:Bundle, event:BundleEvent, obj:Object) {
      }
    })
    val propsService = new PropsService() {
      def updated(updatedProps:Dictionary[String,String]) {
        props = Some(new Props(Util.dictionaryToMap(updatedProps)))
        bundleTracker.open
      }
    }
    context.registerService(classOf[PropsService].getName, propsService, null)

    val broadcaster = new ReceiversBroadcaster()
    val receiverTracker = new OSGIServiceTracker(context, classOf[Receiver].getName, new ServiceTrackerCustomizer {
      def addingService(ref: ServiceReference) = {
        val receiver = context.getService(ref).asInstanceOf[Receiver]
        broadcaster.addReceiver(ref, receiver)
        "KEEP"
      }
      def modifiedService(ref: ServiceReference, x: AnyRef) {}
      def removedService(ref: ServiceReference, x: AnyRef) {
        broadcaster.removeReceiver(ref)
      }
    })
    receiverTracker.open()
    context.registerService(classOf[Broadcaster].getName, broadcaster, null)
  }

  def stop(context:OSGIBundleContext) {
    if (bundleTracker != null) {
      bundleTracker.getBundles.foreach { bundle =>
        bundleTracker.getObject(bundle) match {
          case handle:BundleHandle => handle.holder.map(_.stop)
        }
      }
      bundleTracker.close()
    }
  }
}

object Util {
  def mapToDictionary(map:Map[String,AnyRef]):Dictionary[_,_] = {
    val table = new Hashtable[String,AnyRef]()
    map.foreach ( kv => table.put(kv._1, kv._2))
    table
  }
  def dictionaryToMap[K,V](dictionary:Dictionary[_,_]):Map[K,V] = {
    var map = Map[K,V]()
    val enumeration = dictionary.keys
    while (enumeration.hasMoreElements) {
      val key = enumeration.nextElement
      val value = dictionary.get(key)
      map = map.updated(key.asInstanceOf[K], value.asInstanceOf[V])
    }
    map
  }

  def servicePropertiesToDictionary(properties:List[ServiceProperty]) = {
    mapToDictionary(properties.map(p => p.name -> p.value).toMap)
  }

}


class ServiceProxy[T](context:OSGIBundleContext, klass:Class[T], properties:List[ServiceProperty]) {
  val tracker:OSGIServiceTracker = new OSGIServiceTracker(context, klass.getName, new ServiceTrackerCustomizer {
    def addingService(p1: ServiceReference) = { println("added tracked service: " + klass.getClassLoader + " " + p1); tracker.addingService(p1) }
    def removedService(p1: ServiceReference, p2: AnyRef) { println("removed tracked service: " + klass + " " + p1) }
    def modifiedService(p1: ServiceReference, p2: AnyRef) {}
  })
  def start = tracker.open
  def stop = tracker.close
  def await = tracker.waitForService(0) //0 means forever
  val proxy:T =
      Proxy.newProxyInstance(klass.getClassLoader, Array(klass), new InvocationHandler() {
        def invoke(proxy:Object, method:Method, args:Array[Object]) = {
          val service = tracker.getServices.last//tracker.waitForService(10*1000)
          if (service == null) throw new NoServiceFoundException(10)
          if (!klass.isAssignableFrom(service.getClass)) {
            println("ref " + tracker.getServiceReference)
            println(service.getClass + " from " + service.getClass.getClassLoader)
            println(klass + " from " + klass.getClassLoader)
          }
          method.invoke(service, args : _*)
        }
      }).asInstanceOf[T]
}
class ServiceTracker(context:OSGIBundleContext, klass:Option[Class[_]], properties:List[ServiceProperty], tracker:BromptonServiceTracker) {

  val filters = klass.map(k => "(objectClass="+k.getName+")").toList ::: properties.map { sp => "("+sp.name+"="+sp.value+")"}
  val filter = context.createFilter(filters.head)
  private val osgiTracker:OSGIServiceTracker = new OSGIServiceTracker(context, filter, new ServiceTrackerCustomizer {
    def addingService(ref: ServiceReference) = {
      val klassNames = ref.getProperty(Constants.OBJECTCLASS).asInstanceOf[Array[String]].toList
      val service = context.getService(ref)
      println("Classloader of addingserice " + ref + " " + klassNames.toList)
      tracker.serviceAdded(BromptonServiceReference(ref.toString, klassNames), service)
      osgiTracker.addingService(ref)
    }

    def removedService(ref: ServiceReference, x: AnyRef) {
      val klassNames = ref.getProperty(Constants.OBJECTCLASS).asInstanceOf[Array[String]].toList
      tracker.serviceRemoved(BromptonServiceReference(ref.toString, klassNames))
    }

    def modifiedService(ref: ServiceReference, x: AnyRef) {
    }
  })
  osgiTracker.open
}
class OSGIBromptonContext(val context:OSGIBundleContext) extends BromptonContext {
  private val serviceProxies = new ConcurrentHashMap[Class[_],ServiceProxy[_]]()
  def nameThread[T](name:String, f:()=>T):T = {
    val currentName = Thread.currentThread.getName
    Thread.currentThread.setName(currentName + " > " + name)
    val r = f()
    Thread.currentThread.setName(currentName)
    r
  }
  def createServiceTracker(klass:Option[Class[_]], properties:List[ServiceProperty], tracker:BromptonServiceTracker) {
    new ServiceTracker(context, klass, properties, tracker)
  }
  def awaitService[T](klass:Class[T]):T = {
    val myProxy = new ServiceProxy(context, klass, Nil)
    val r = serviceProxies.putIfAbsent(klass, myProxy)
    val serviceProxy = if (r == null) {
      myProxy.start
      myProxy
    } else {
      r.asInstanceOf[ServiceProxy[T]]
    }
    serviceProxy.await
    serviceProxy.proxy.asInstanceOf[T]
  }
  def registerService[T](klass:Class[T], service:T, properties:List[ServiceProperty]=List()) = {
    if (service == null) throw new Exception("Service is null")
    if (klass== null) throw new Exception("class is null")
    val ref = context.registerService(
        klass.getName,
        service.asInstanceOf[Object],
        Util.servicePropertiesToDictionary(properties)
    )
    new BromptonServiceRegistration() {
      def unregister() {
        ref.unregister()
      }
    }
  }
}
class BundleActivatorHolder(activator:BromptonActivator, context:OSGIBromptonContext, latch:CountDownLatch) {
  private var thread:Thread = _
  def doStart(props:Props) {
    thread = new Thread(new Runnable() { def run() {
      try {
        activator.start(context)
        val classLoader = /*BundleDelegatingClassLoader.createBundleClassLoaderFor(
            context.context.getBundle,*/
          classOf[Props].getClassLoader//)
        activator.init(context, props.applyOverrides(activator.getClass.getClassLoader, activator.defaults))
      } catch {
        case e:InterruptedException => {
          e.printStackTrace()
          /*caused by stop when still waiting*/
        }
        case e => {
          e.printStackTrace()
        }
      } finally {
        latch.countDown()
      }
    } })
    thread.setName("Activator " + activator.getClass.getName)
    thread.setDaemon(true)
    thread.start()
  }
  def props(props:Props) {
    val t = new Thread(new Runnable() { def run() {
      try {
        val classLoader = /*BundleDelegatingClassLoader.createBundleClassLoaderFor(
            context.context.getBundle,*/
          classOf[Props].getClassLoader//)
        activator.init(context, props.applyOverrides(activator.getClass.getClassLoader, activator.defaults))
      } catch {
        case e => e.printStackTrace()
      }
    } })
    t.start()
  }
  def stop() {
    thread.interrupt()
    activator.stop(context)
  }
}