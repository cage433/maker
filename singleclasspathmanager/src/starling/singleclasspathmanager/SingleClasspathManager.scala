package starling.singleclasspathmanager

import starling.manager._

class SingleClasspathManager(properties:Map[String,String], activators:List[Class[_ <: BromptonActivator]]) {
  val props = new Props(properties)
  case class ServiceEntry(klass:Class[_], service:AnyRef, properties:List[ServiceProperty], reference:BromptonServiceReference) {
    private val propertiesSet = properties.toSet
    def hasProperties(predicate:List[ServiceProperty]) = predicate.forall(p=>propertiesSet.contains(p))
  }

  case class TrackerEntry(klass:Option[Class[_]], properties:List[ServiceProperty], tracker:BromptonServiceTracker) {

    def applyTo(services:List[ServiceEntry]) {
      val matches = services.filter { reg => reg.hasProperties(properties) && klass.map(_ == reg.klass).getOrElse(true) }
      matches.foreach { reg => tracker.serviceAdded(reg.reference, reg.service) }
    }
  }

  var id = 0
  private var started = false
  private val instances = activators.map(_.newInstance)
  private val registry = new scala.collection.mutable.ArrayBuffer[ServiceEntry]()
  private val trackers = new scala.collection.mutable.ArrayBuffer[TrackerEntry]()
  private val context = new BromptonContext() {
    def registerService[T](
      klass:Class[T],
      service:T,
      properties:List[ServiceProperty]=List()) = {
      if (!klass.isAssignableFrom(service.asInstanceOf[AnyRef].getClass)) throw new Exception(service + " is not a " + klass)
      val ref = { id+=1; BromptonServiceReference(id + ":" + klass) }
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
      registry.toList.filter(_.klass == klass) match {
        case Nil => throw new NoServiceFoundException(0)
        case entry :: Nil => entry.service.asInstanceOf[T]
        case many => throw new Exception("There is more than one " + klass + " service")
      }
    }
    def createServiceTracker(klass:Option[Class[_]], properties:List[ServiceProperty], tracker:BromptonServiceTracker):Unit = {
      val trackerEntry = TrackerEntry(klass, properties, tracker)
      trackerEntry.applyTo(registry.toList)
      trackers += trackerEntry
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
