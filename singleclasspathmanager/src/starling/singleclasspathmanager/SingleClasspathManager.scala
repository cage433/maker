package starling.singleclasspathmanager

import starling.manager._

class SingleClasspathManager(properties:Map[String,String], activators:List[Class[_ <: BromptonActivator]]) {
  val props = new Props(properties)
  case class ServiceEntry(klass:Class[_], service:AnyRef, properties:List[ServiceProperty], reference:BromptonServiceReference) {
    private val propertiesSet = properties.toSet
    def hasProperties(predicate:List[ServiceProperty]) = predicate.forall(p=>propertiesSet.contains(p))
  }

  case class TrackerEntry[T](klass:Option[Class[_]], properties:List[ServiceProperty], callback:BromptonServiceCallback[T]) {

    def applyTo(services:List[ServiceEntry]) {
      each { (ref, service) => callback.serviceAdded(ref, service) }
    }

    def each(f:( (BromptonServiceReference,T)=>Unit) ) {
      val services = registry.toList
      val matches = services.filter { reg => reg.hasProperties(properties) && klass.map(_ == reg.klass).getOrElse(true) }
      matches.foreach { reg => f(reg.reference, reg.service.asInstanceOf[T]) }
    }
  }

  def service[T](klass:Class[T]) = {
    registry.toList.filter(_.klass == klass) match {
      case Nil => throw new NoServiceFoundException("No " + klass + " service found")
      case entry :: Nil => entry.service.asInstanceOf[T]
      case many => throw new Exception("There is more than one " + klass + " service")
    }
  }

  var id = 0
  private var started = false
  private val instances = activators.map(_.newInstance)
  private val registry = new scala.collection.mutable.ArrayBuffer[ServiceEntry]()
  private val trackers = new scala.collection.mutable.ArrayBuffer[TrackerEntry[_]]()
  private val context = new BromptonContext() {
    def registerService[T](
      klass:Class[T],
      service:T,
      properties:List[ServiceProperty]=List()) = {
      if (!klass.isAssignableFrom(service.asInstanceOf[AnyRef].getClass)) throw new Exception(service + " is not a " + klass)
      val ref = { id+=1; BromptonServiceReference(id + ":" + klass, List(klass.getName)) }
      val entry = ServiceEntry(klass, service.asInstanceOf[AnyRef], properties, ref)
      registry.append( entry )

      trackers.toList.foreach{ tracker => {
        tracker.applyTo(List(entry))
      }}
      new BromptonServiceRegistration() {
        def unregister() { throw new Exception("Unsupported") }
      }
    }
    def awaitService[T](klass:Class[T]):T = {
      service(klass)
    }
    def createServiceTracker[T](klass:Option[Class[T]], properties:List[ServiceProperty], callback:BromptonServiceCallback[T]) = {
      val trackerEntry = TrackerEntry(klass, properties, callback)
      trackerEntry.applyTo(registry.toList)
      trackers += trackerEntry
      new BromptonServiceTracker[T] {
        def each(f: (T) => Unit) {
          trackerEntry.each { (ref,service) => f(service) }
        }
      }
    }
  }

  def start() {
    this synchronized {
      if (started) throw new Exception("Already started")
      started = true
      val classLoader = classOf[SingleClasspathManager].getClassLoader
      instances.foreach { activator => {
        activator.start(context)
        activator.init(context, props.applyOverrides(classLoader, activator.defaults))
      } }
      props.completed()
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
