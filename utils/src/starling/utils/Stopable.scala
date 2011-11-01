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

trait Stopable extends Startable {
  def stop { running.set(false) }
  def close() = stop
}

object Stopable {
  val NULL = new Stopable { }
  def createIf(condition: => Boolean)(creator: => Stopable): Stopable = if (condition) creator else NULL
  def apply(startF: () => Any = () => (), stopF: () => Any = () => ()): Stopable = new Stopable {
    override def start = { super.start; startF() }
    override def stop  = { super.stop;  stopF()  }
  }
}

case class CompositeStopable(stopables: (Boolean, List[Stopable])*) extends Stopable {
  private val enabled = stopables.filter(_._1).flatMap(_._2)

  override def start = { super.start; enabled.map(_.start)  }
  override def stop =  { super.stop;  enabled.map(_.stop)   }
}

trait Enableable {
  private val enabled = new AtomicBoolean(false)

  def enable = enabled.set(true)
  def disable = enabled.set(false)
  def isEnabled = enabled.get
}