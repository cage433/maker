package starling.calendar

import starling.daterange.Timestamp


trait Clock {
  def timestamp: Timestamp
}

object Clock extends Clock {
  private var clock: Clock = SystemClock

  def freeze {
    clock = FrozenClock(new Timestamp { override def toString = "Frozen: " + super.toString })
  }

  def thaw {
    clock = SystemClock
  }

  def timestamp = clock.timestamp

  private case object SystemClock extends Clock {
    def timestamp = Timestamp.now
  }
  private case class FrozenClock(timestamp: Timestamp) extends Clock
}

