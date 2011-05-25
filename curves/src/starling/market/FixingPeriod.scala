package starling.market

import starling.daterange._


trait FixingPeriod {
  def period(day:Day):DateRange
  def storedPeriod: StoredFixingPeriod
}

case class IndexFixingPeriod(index:SingleIndex, storedPeriod:StoredFixingPeriod) extends FixingPeriod {
  def period(day:Day) = index.observedPeriod(day)
}

case class DateRangeFixingPeriod(dateRange:DateRange) extends FixingPeriod {
  def period(day: Day) = dateRange
  def storedPeriod = new StoredFixingPeriod(Left(dateRange))
}
