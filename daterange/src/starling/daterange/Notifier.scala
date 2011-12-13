package starling.daterange

import starling.calendar.Clock
import collection.mutable.{Map => MMap}
import starling.utils.ImplicitConversions._
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}


trait Notifier {
  def notify[T](name: String)
  def expectWithin(name: String, delay: Long, errorCallback: () => Any)
}

object Notifier {
  object Null extends Notifier {
    def notify[T](name: String) {}
    def expectWithin(name: String, delay: Long, errorCallback: () => Any) {}
  }
}

class TimedNotifier(scheduledExecutorService: ScheduledExecutorService) extends Notifier {
  private val expectations = MMap.empty[String, Expectation]
  
  def notify[T](name: String) = synchronized {
    expectations.remove(name)
  }

  def expectWithin(name: String, delay: Long, errorCallback: () => Any): Unit = synchronized {
    val expiry = Clock.timestamp + new Duration(delay, TimeUnit.MILLISECONDS)
    
    expectations.update(name, Expectation(expiry, errorCallback))

    scheduledExecutorService.schedule(new Poll, delay, TimeUnit.MILLISECONDS)
  }

  private def verify = synchronized {
    expectations.filterValues(_.verify)
  }
  
  private case class Expectation(expiry: Timestamp, errorCallback: () => Any) {
    def expired = Clock.timestamp >= expiry

    def verify: Boolean = if (expired) {
      errorCallback()
      false
    } else true
  }

  private class Poll extends Runnable {
    def run = verify
  }
}