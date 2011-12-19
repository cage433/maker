package starling.daterange

import java.util.Date
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

case class Timestamp(instant : Long) extends Ordered[Timestamp] {

  def this() = this(new Date().getTime)

  private val dt = new DateTime(instant)
  def toDateTime = dt

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
  def toStringSecondsShort = dt.toString("ddMMMyy HH:mm ss")
  def timeString = dt.toString("HH:mm")
  def timeStringWithSeconds = dt.toString("HH:mm ss")
  def timeStringWithMilliSeconds = dt.toString("HH:mm:ss.SSS")

  override def compare(rhs : Timestamp) : Int = {
    if (instant < rhs.instant) -1 else if (instant > rhs.instant) 1 else 0
  }
  override def toString = dt.toString(Timestamp.pattern)
  def max (that : Timestamp) = if (this > that) this else that
  def min (that : Timestamp) = if (this < that) this else that

  def toDay : Day = Day.fromJavaDate(toJavaDate)
}

object Timestamp {
  val patterns = List("ddMMMyyyy HH:mm:ss.SSS", "yyyy-MM-dd HH:mm:ss.SSS")
  val pattern = patterns.head

  def now = new Timestamp

  def parse(str: String): Timestamp = patterns.flatMap {
    pattern =>
      try {
        val fmt = DateTimeFormat.forPattern(pattern)
        val dateTime: DateTime = fmt.parseDateTime(str)
        Some(Timestamp(dateTime.getMillis))
      }
      catch {
        case e: IllegalArgumentException => None
      }
  }.headOption.getOrElse(throw new IllegalArgumentException("Failed to parse " + str + " as Timestamp."))

  implicit object ordering extends Ordering[Timestamp]{
    def compare(lhs : Timestamp, rhs : Timestamp) : Int = lhs.compare(rhs)
  }
}
