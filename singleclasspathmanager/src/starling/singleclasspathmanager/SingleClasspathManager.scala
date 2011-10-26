package starling.singleclasspathmanager

import starling.manager._
import starling.osgimanager.utils.ThreadSafeCachingProxy

class SingleClasspathManager(cacheServices:Boolean, activators:List[Class[_ <: BromptonActivator]], initialServices:List[(Class[_],AnyRef)]=Nil) {
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

  def service[T](klass:Class[T]) = {
    registry.toList.filter(_.klass == klass) match {
      case Nil => throw new NoServiceFoundException("No " + klass + " service found")
      case entry :: Nil => entry.service.asInstanceOf[T]
      case many => throw new Exception("There is more than one " + klass + " service")
    }
  }

  private def register(klass:Class[_], service:AnyRef, properties:ServiceProperties) {
    if (!klass.isAssignableFrom(service.asInstanceOf[AnyRef].getClass)) throw new Exception(service + " is not a " + klass)
    val ref = { id+=1; BromptonServiceReference(id + ":" + klass, List(klass.getName)) }
    val entry = ServiceEntry(klass, service.asInstanceOf[AnyRef], properties, ref)
    registry.append( entry )
    trackers.toList.foreach{ tracker => {
      tracker.applyTo(List(entry))
    }}
  }

  var id = 0
  private var started = false
  private val instances = activators.map(_.newInstance)
  private val registry = new scala.collection.mutable.ArrayBuffer[ServiceEntry]()
  private val trackers = new scala.collection.mutable.ArrayBuffer[TrackerEntry[_]]()
  private val onStartedActions = new scala.collection.mutable.ArrayBuffer[() => Unit]()

  initialServices.foreach { case (klass,instance) => register(klass, instance, ServiceProperties())}

  private val context = new BromptonContext() {
    def registerService[T](
      klass:Class[T],
      service:T,
      properties:ServiceProperties = ServiceProperties()) = {
      val cachingService = if (cacheServices && klass.isInterface) ThreadSafeCachingProxy.createProxy(klass, service) else service
      register(klass, cachingService.asInstanceOf[AnyRef], properties)
      new BromptonServiceRegistration() {
        def unregister() { throw new Exception("Unsupported") }
      }
    }
    def awaitService[T](klass:Class[T]):T = {
      service(klass)
    }
    def createServiceTracker[T](klass:Option[Class[T]], properties:ServiceProperties, callback:BromptonServiceCallback[T]) = {
      val trackerEntry = TrackerEntry(klass, properties, callback)
      trackerEntry.applyTo(registry.toList)
      trackers += trackerEntry
      new BromptonServiceTracker[T] {
        def each(f: (T) => Unit) {
          trackerEntry.each(registry.toList) { (ref,properties,service) => f(service) }
        }
      }
    }

    def onStarted(action: => Unit) = onStartedActions += (() => action)
  }


  def start() {
    this synchronized {
      if (started) throw new Exception("Already started")
      started = true
      val classLoader = classOf[SingleClasspathManager].getClassLoader
      instances.foreach { activator => {
        activator.start(context)
      } }

      onStartedActions.foreach { action => try {
        action()
      } catch {
        case t => t.printStackTrace(Console.err)
      } }
    }
  }
  def stop() {
    this synchronized {
      if (!started) throw new Exception("Not started yet")
      started = false
	  instances.foreach { activator => {
	    activator.stop(context)
	  } }
    }
  }
}
