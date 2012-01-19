package starling.singleclasspathmanager

import starling.manager._
import starling.osgimanager.utils.ThreadSafeCachingProxy

class SingleClasspathManager(cacheServices:Boolean, activators:List[Class[_ <: BromptonActivator]], initialServices:List[(Class[_],AnyRef)]=Nil) {

  val lock = new Object()

  case class ServiceEntry(klass:Class[_], service:AnyRef, properties:ServiceProperties, reference:BromptonServiceReference) {
    def hasProperties(predicate:ServiceProperties) = properties.contains(predicate)
  }

  case class TrackerEntry[T](klass:Option[Class[_]], properties:ServiceProperties, callback:BromptonServiceCallback[T]) {

    def applyTo(services:List[ServiceEntry]) {
      each(services) { (ref, properties, service) => callback.serviceAdded(ref, properties, service) }
    }

    def each(services:List[ServiceEntry])(f:( (BromptonServiceReference,ServiceProperties, T)=>Unit) ) {
      val matches = services.filter { reg => reg.hasProperties(properties) && klass.map(_ == reg.klass).getOrElse(true) }
      matches.foreach { reg => f(reg.reference, reg.properties, reg.service.asInstanceOf[T]) }
    }
  }

  def service[T](klass:Class[T]):T = {
    lock.synchronized {
      registry.toList.filter(_.klass == klass) match {
        case Nil => {
          lock.wait() //wait then try again when something is registered
          service(klass)
        }
        case entry :: Nil => entry.service.asInstanceOf[T]
        case many => throw new Exception("There is more than one " + klass + " service")
      }
    }
  }

  private def register(klass:Class[_], service:AnyRef, properties:ServiceProperties) {
    lock.synchronized {
      if (!klass.isAssignableFrom(service.asInstanceOf[AnyRef].getClass)) throw new Exception(service + " is not a " + klass)
      val ref = { id+=1; BromptonServiceReference(id + ":" + klass, List(klass.getName)) }
      val entry = ServiceEntry(klass, service.asInstanceOf[AnyRef], properties, ref)
      registry.append( entry )
      trackers.toList.foreach{ tracker => {
        tracker.applyTo(List(entry))
      }}
      lock.notifyAll()
    }
  }

  var id = 0
  private var started = false
  private val instances: List[BromptonActivator] = activators.map(_.newInstance)
  private val registry = new scala.collection.mutable.ArrayBuffer[ServiceEntry]()
  private val trackers = new scala.collection.mutable.ArrayBuffer[TrackerEntry[_]]()
  private val onStartedActions = new scala.collection.mutable.ArrayBuffer[() => Unit]()
  private val onStoppedActions = new scala.collection.mutable.ArrayBuffer[() => Unit]()

  initialServices.foreach { case (klass,instance) => register(klass, instance, ServiceProperties())}

  private val context = new BromptonContext() {
    def registerService[T](
      klass:Class[T],
      service:T,
      properties:ServiceProperties = ServiceProperties()) = {

      val (cachingService, props) = (cacheServices && klass.isInterface, asRef(service)) match {
        case (true, Some(aService)) =>
          (ThreadSafeCachingProxy.createProxy(klass, service), properties + ProxiedService(aService.getClass))
        case _ => (service, properties)
      }

      register(klass, cachingService.asInstanceOf[AnyRef], props)
      new BromptonServiceRegistration() {
        def unregister() { throw new Exception("Unsupported") }
      }
    }
    def awaitService[T](klass:Class[T]):T = {
      service(klass)
    }
    def createServiceTracker[T](klass:Option[Class[T]], properties:ServiceProperties, callback:BromptonServiceCallback[T]) = {
      lock.synchronized {
        val trackerEntry = TrackerEntry(klass, properties, callback)
        trackerEntry.applyTo(registry.toList)
        trackers += trackerEntry
        new BromptonServiceTracker[T] {
          def each(f: (T) => Unit) {
            trackerEntry.each(registry.toList) { (ref,properties,service) => f(service) }
          }
        }
      }
    }

    def onStarted(action: => Unit) {
      lock.synchronized {
        onStartedActions += (() => action)
      }
    }
    def onStopped(action: => Unit) {
      lock.synchronized {
        onStoppedActions += (() => action)
      }
    }
  }

  def start() {
    if (started) throw new Exception("Already started")
    started = true
    val threads = instances.map { activator => {
      val thread = new Thread(new Runnable() { def run() {
        try {
          activator.start(context)
        } catch {
          case e:Exception => {
            e.printStackTrace()
            println("Exiting because " + activator + " failed")
            System.exit(1)
          }
        }
      } })
      thread.start()
      thread
    } }
    threads.foreach(_.join())
    lock.synchronized {
      onStartedActions.foreach(printExceptions)
    }
  }

  def stop() {
    lock synchronized {
      if (!started) throw new Exception("Not started yet")
      started = false
      onStoppedActions.foreach(printExceptions)
    }
  }

  private def printExceptions(action: () => Unit) = try {
    action()
  } catch {
    case t => t.printStackTrace(Console.err)
  }

  private def asRef(any: Any): Option[AnyRef] = if (any.isInstanceOf[AnyRef]) Some(any.asInstanceOf[AnyRef]) else None
}
