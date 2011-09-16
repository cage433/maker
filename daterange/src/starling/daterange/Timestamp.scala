package starling.daterange

import java.util.Date
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

case class Timestamp(instant : Long) extends Ordered[Timestamp] {

  def this() = this(new Date().getTime)

  private val dt = new DateTime(instant)

  def toJavaDate = new Date(instant)

  def day = Day.fromMillis(instant)
  def month = {
    val d = day
    Month(d.year, d.month)
  }

  def next = Timestamp(instant + 1)

  def - (duration : Duration) = Timestamp(instant - duration.millis)
  def + (duration : Duration) = Timestamp(instant + duration.millis)

  def roundDownToClosestSecond = Timestamp(dt.withMillisOfSecond(0).getMillis)
  def toStringMinutes = dt.toString("ddMMMyyyy HH:mm")
  def toStringSeconds = dt.toString("ddMMMyyyy HH:mm ss")
  def timeString = dt.toString("HH:mm")
  def timeStringWithSeconds = dt.toString("HH:mm ss")

  override def compare(rhs : Timestamp) : Int = {
    if (instant < rhs.instant) -1 else if (instant > rhs.instant) 1 else 0
  }
  override def toString = dt.toString(Timestamp.pattern)
  def max (that : Timestamp) = if (this > that) this else that
  def min (that : Timestamp) = if (this < that) this else that

  def toDay : Day = Day.fromJavaDate(toJavaDate)
}

object Timestamp {
  val pattern = "ddMMMyyyy HH:mm:ss.SSS"

  def now = new Timestamp

  def parse(str : String) : Timestamp = {
    val fmt = DateTimeFormat.forPattern(pattern)
    val dateTime : DateTime = fmt.parseDateTime(str)
    Timestamp(dateTime.getMillis)
  }

  implicit object ordering extends Ordering[Timestamp]{
    def compare(lhs : Timestamp, rhs : Timestamp) : Int = lhs.compare(rhs)
  }
}
