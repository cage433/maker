package starling.utils

import java.util.concurrent.atomic.AtomicBoolean

trait Startable {
  protected val running = new AtomicBoolean(false)
  def isRunning = running.get
  def start { running.set(true) }
}

object Startable {
  val NULL = new Startable {}
}

trait Stoppable extends Startable {
  def stop { running.set(false) }
  def close() = stop
}

object Stoppable {
  val NULL = new Stoppable { }
  def createIf(condition: => Boolean)(creator: => Stoppable): Stoppable = if (condition) creator else NULL
}