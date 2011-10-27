package starling.calendar

import starling.daterange.{Day, Timestamp}
import scalaz.Scalaz._

trait Clock {
  def timestamp: Timestamp
  def today: Day
}

object Clock extends Clock {
  private var clock: Clock = SystemClock

  def freezeTo(day: Option[Day]) {
    clock = FrozenClock(day.fold(_.millis, System.currentTimeMillis()))
  }

  def freeze = freezeTo(None)

  def thaw {
    clock = SystemClock
  }

  def timestamp = clock.timestamp
  def today = clock.today

  private case object SystemClock extends Clock {
    def timestamp = Timestamp.now
    def today = Day.fromMillis(System.currentTimeMillis())
  }
  private case class FrozenClock(instant: Long) extends Clock {
    val timestamp = new FrozenTimestamp(instant)
    val today = timestamp.toDay

    class FrozenTimestamp(instant: Long) extends Timestamp(instant) {
      override def toString = "Frozen: " + super.toString
    }
  }
}

