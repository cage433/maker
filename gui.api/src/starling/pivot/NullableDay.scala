package starling.pivot

import starling.daterange._

case class NullableDay(day:Day, t:Throwable) extends Ordered[NullableDay] {
  override def toString = if (day!=null) day.toString else if (t!=null) "E" else "n/a"
  def compare(that: NullableDay) = {
    val thisPosition = if (this.day==null) if (this.t==null) -1 else 0 else this.day.julianDayNumber
    val thatPosition = if (that.day==null) if (that.t==null) -1 else 0 else that.day.julianDayNumber
    thisPosition - thatPosition
  }
}
object NullableDay {
  def apply(day:Day):NullableDay = NullableDay(day, null)
  def error(t:Throwable) = NullableDay(null, t)
  val Null = new NullableDay(null, null)
}

class NullableDayFieldDetails(name:String) extends FieldDetails(Field(name)) {
  override def nullValue = NullableDay.Null
}

case class NullablePeriod(period: Period, error:Boolean) extends Ordered[NullablePeriod] {
  override def toString = {
    if (error) {
      "Error"
    } else if (period == null) {
      "n/a"
    } else {
      period.toString
    }
  }
  def compare(that:NullablePeriod) = {
    if (error && that.error) 0
    else if (error) -1
    else if (that.error) 1
    else if (period == null) -1
    else if (that.period == null) 1
    else period.compare(that.period)
  }
}
object NullablePeriod {
  def parse(period:String):NullablePeriod = period match {
    case Period(p) => NullablePeriod(p, false)
    case _ => NullablePeriod(null, true)
  }
  def error(t:Throwable) = NullablePeriod(null, true)
  val Null = NullablePeriod(null, false)
}