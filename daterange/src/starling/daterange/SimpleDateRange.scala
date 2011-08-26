package starling.daterange

/**
 * arbitrary contiguous range of days.
 */
case class SimpleDateRange(firstDay : Day, lastDay : Day) extends DateRange {
  require(firstDay <= lastDay, "Bad date order: from = " + firstDay + ", to = " + lastDay)

  def tenor = None

  override def toString = firstDay + " - " + lastDay

  def toListOfMonths = {
    if(firstDay == firstDay.containingMonth.firstDay && lastDay == lastDay.containingMonth.lastDay) {
      days.toList.map(d => Month(d.year, d.month)).distinct
    } else {
      throw new Exception ("SimpleDateRange " + this + " can't be broken into months")
    }
  }

  def +(i: Int) = new SimpleDateRange(firstDay + i, lastDay + i)
  def -(n: Int) = this + (-n)
}

object SimpleDateRange{
  def containingRange(days : Seq[Day]) = {
    val sortedDays = days.sortWith(_<_)
    SimpleDateRange(sortedDays.head, sortedDays.last)
  }
}
