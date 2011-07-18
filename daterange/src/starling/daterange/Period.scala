package starling.daterange

import starling.utils.StringToDouble

trait Period extends Ordered[Period] {
  def toList: List[DateRange]
}

object Period {
  implicit def dateRangeToPeriod(daterange: DateRange) = DateRangePeriod(daterange)

  implicit def spreadToPeriod[T <: DateRange](spread: Spread[T]) = SpreadPeriod(spread.first, spread.last)

  implicit def stripToPeriod[T <: Period](strip: Strip[T]) = StripPeriod(strip.first, strip.last)

  def unapply(a: Any): Option[Period] = a match {
    case d: Double => Some(Day.fromExcel(d))
    case StringToDouble(d) => Some(Day.fromExcel(d))
    case a => a.toString match {
      case SpreadParse(s) => Some(s)
      case StripParse(s) => Some(s)
      case DateRange(d) => Some(d)
      case TenorType(t) => Some(t)
      case Day(d) => Some(d)
      case _ => None
    }}
}

case class DateRangePeriod(period: DateRange) extends Period {
  override def toString = period.toShortString

  def next: DateRangePeriod = period.tenor match {
    case Some(t) => DateRangePeriod(t.add(1, period.asInstanceOf[t.T]))
    case None => throw new Exception("Can't call `next` on " + this)
  }

  def toList = List(period)

  override def compare(that: Period) = (this, that) match {
    case (DateRangePeriod(lPeriod), DateRangePeriod(rPeriod)) => lPeriod.compare(rPeriod)
  }
}

case class SpreadPeriod(front: DateRange, back: DateRange) extends Period {
  override def toString = front.toShortString + "/" + back.toShortString

  def next = {
    val n = Spread(front, back).next
    SpreadPeriod(n.first, n.last)
  }

  def toList = List(front, back)

  override def compare(that: Period) = (this, that) match {
    case (SpreadPeriod(lFront, lBack), SpreadPeriod(rFront, rBack)) =>
      lFront.compare(rFront) match {
        case 0 => lBack.compare(rBack)
        case x => x
      }
  }
}

case class StripPeriod(first: Period, last: Period) extends Period {
  override def toString = first.toString + "-" + last.toString

  def toList = Strip(first, last).toList.flatMap(_.toList)

  override def compare(that: Period) = (this, that) match {
    case (StripPeriod(lFront, lBack), StripPeriod(rFront, rBack)) =>
      // this works because we can't have strips of strips
      lFront.compare(rFront) match {
        case 0 => lBack.compare(rBack)
        case x => x
      }
  }
}
