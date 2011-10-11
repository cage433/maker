package starling.osgimanager

import starling.manager.BromptonServiceReference._
import org.osgi.framework.{BundleActivator => OSGIBundleActivator}
import org.osgi.framework.{BundleContext => OSGIBundleContext}
import org.osgi.framework.ServiceReference
import org.osgi.framework._
import java.io.{FileInputStream, File}
import java.util.{Dictionary,Hashtable,Properties}
import org.osgi.framework.Bundle
import org.osgi.framework.BundleEvent
import org.osgi.service.cm.ManagedService
import org.osgi.service.cm.ConfigurationAdmin
import swing.Publisher
import org.osgi.util.tracker.{ServiceTrackerCustomizer, ServiceTracker => OSGIServiceTracker, BundleTracker, BundleTrackerCustomizer}
import java.util.concurrent.{CountDownLatch, ConcurrentHashMap}
import swing.event.Event
import collection.JavaConversions
import net.sf.cglib.proxy.{MethodProxy, MethodInterceptor, Enhancer}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import starling.manager._
import starling.utils.{Receiver, Broadcaster, ReceiversBroadcaster}
import java.lang.reflect.{Method,UndeclaredThrowableException,InvocationTargetException}
import starling.utils.cache.CacheFactory
import utils.ThreadSafeCachingProxy

trait PropsService {
  def updated(props:Dictionary[String,String])
}

class OsgiManager

class BundleHandle(var holder:Option[BundleActivatorHolder])

class BromptonOSGIActivator extends OSGIBundleActivator {
  var bundleTracker:BundleTracker = _
  val serviceIDSequence = new java.util.concurrent.atomic.AtomicInteger()
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
          val context = new OSGIBromptonContext(serviceIDSequence, bundle.getBundleContext)
          val holder = new BundleActivatorHolder(activator, context, latch)
          println(">>Starting: " + bundle.getSymbolicName)
          holder.doStart()
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
    bundleTracker.open

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

  override def stop(context:OSGIBundleContext) {
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

//object Util {
//  def mapToDictionary(map:Map[String,AnyRef]):Dictionary[_,_] = {
//    val table = new Hashtable[String,AnyRef]()
//    map.foreach ( kv => table.put(kv._1, kv._2))
//    table
//  }
//  def dictionaryToMap[K,V](dictionary:Dictionary[_,_]):Map[K,V] = {
//    var map = Map[K,V]()
//    val enumeration = dictionary.keys
//    while (enumeration.hasMoreElements) {
//      val key = enumeration.nextElement
//      val value = dictionary.get(key)
//      map = map.updated(key.asInstanceOf[K], value.asInstanceOf[V])
//    }
//    map
//  }
//}


class ServiceProxy[T](context:OSGIBundleContext, klass:Class[T], properties:List[ServiceProperty]) {
  val tracker:OSGIServiceTracker = new OSGIServiceTracker(context, klass.getName, new ServiceTrackerCustomizer {
    def addingService(p1: ServiceReference) = { println("added tracked service: " + klass.getClassLoader + " " + p1); tracker.addingService(p1) }
    def removedService(p1: ServiceReference, p2: AnyRef) { println("removed tracked service: " + klass + " " + p1) }
    def modifiedService(p1: ServiceReference, p2: AnyRef) {}
  })
  def start = tracker.open
  def stop = tracker.close
  def await = tracker.waitForService(0) //0 means forever
  def proxy:T = {
    if (!klass.isInterface) {
      val s = tracker.waitForService(10*1000).asInstanceOf[T]
      if (s == null) {
        println("Have been waiting 10 seconds for a " + klass + " still waiting...")
        val s1 = tracker.waitForService(0).asInstanceOf[T]
        println("Got a " + klass)
        s1
      } else {
        s
      }
    } else {
      val e = new Enhancer()
      e.setClassLoader(klass.getClassLoader)//classOf[Props].getClassLoader)
      e.setSuperclass(klass)
      e.setCallback(new MethodInterceptor() {
         def intercept(obj:Object, method:Method,
                      args:Array[Object], proxy:MethodProxy):Object = {

           val service = tracker.waitForService(0)//getServices.last//tracker.waitForService(10*1000)
           if (service == null) throw new NoServiceFoundException("No " + klass + " " + properties + " service found")
           if (!klass.isAssignableFrom(service.getClass)) {
             println("ref " + tracker.getServiceReference)
             println(service.getClass + " from " + service.getClass.getClassLoader)
             println(klass + " from " + klass.getClassLoader)
           }
           method.invoke(service, args : _*)
         }
      })
      try {
        e.create().asInstanceOf[T]
      } catch {
        case e:Exception => throw new Exception("Failed to create proxy for " + klass, e)
      }
    }
  }
//      Proxy.newProxyInstance(klass.getClassLoader, Array(klass), new InvocationHandler() {
//        def invoke(proxy:Object, method:Method, args:Array[Object]) = {
//        }
//      }).asInstanceOf[T]
}
class ServiceTracker[T](val serviceIDSequence : AtomicInteger, context:OSGIBundleContext, klass:Option[Class[T]], properties:ServiceProperties, tracker:BromptonServiceCallback[T])
  extends BromptonServiceTracker[T] {

  val filters = klass.map(k => "(objectClass="+k.getName+")").toList ::: properties.toFilters
  val filter = context.createFilter(filters.head)
  private val osgiTracker:OSGIServiceTracker = new OSGIServiceTracker(context, filter, new ServiceTrackerCustomizer {
    def addingService(ref: ServiceReference) = {
      val klassNames = ref.getProperty(Constants.OBJECTCLASS).asInstanceOf[Array[String]].toList
      val serviceID = serviceIDSequence.getAndIncrement
      val service = context.getService(ref)
      val bromptonRef = BromptonServiceReference(serviceID.toString, klassNames)
      tracker.serviceAdded(bromptonRef, ServiceProperties(), service.asInstanceOf[T])
      osgiTracker.addingService(ref)
      bromptonRef
    }

    def removedService(ref: ServiceReference, x: AnyRef) {
      val bromptonRef = x.asInstanceOf[BromptonServiceReference]
      tracker.serviceRemoved(bromptonRef)
    }

    def modifiedService(ref: ServiceReference, x: AnyRef) {
    }
  })
  osgiTracker.open

  def each(f: (T) => Unit) {
    osgiTracker.getServices.foreach { service => f(service.asInstanceOf[T]) }
  }
}
class OSGIBromptonContext(val serviceIDSequence : AtomicInteger, val context:OSGIBundleContext) extends BromptonContext {
  private val serviceProxies = new ConcurrentHashMap[Class[_],ServiceProxy[_]]()
  def nameThread[T](name:String, f:()=>T):T = {
    val currentName = Thread.currentThread.getName
    Thread.currentThread.setName(currentName + " > " + name)
    val r = f()
    Thread.currentThread.setName(currentName)
    r
  }
  def createServiceTracker[T](klass:Option[Class[T]], properties:ServiceProperties, tracker:BromptonServiceCallback[T]) = {
    new ServiceTracker[T](serviceIDSequence, context, klass, properties, tracker)
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
  def registerService[T](klass:Class[T], service:T, properties:ServiceProperties=ServiceProperties()) = {
    if (service == null) throw new Exception("Service is null")
    if (klass== null) throw new Exception("class is null")
    val cachingService = ThreadSafeCachingProxy.createProxy(klass, service)
    val ref = context.registerService(
        klass.getName,
        cachingService.asInstanceOf[Object],
        properties.toDictionary
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
  def doStart() {
    thread = new Thread(new Runnable() { def run() {
      try {
        activator.start(context)
      } catch {
        case e:InterruptedException => {
          e.printStackTrace()
          /*caused by stop when still waiting*/
        }
        case e:Exception => {
          println("Exception while starting " + activator.getClass)
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
  def stop() {
    thread.interrupt()
    activator.stop(context)
  }
}