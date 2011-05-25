package starling.varcalculator

import starling.daterange.TenorType

/**
 * Number of days, month etc. the price is ahead of market day.
 */
case class NAhead(numPeriodsAhead: Int, tenor:String) extends Ordered[NAhead] {
  def num = numPeriodsAhead

  def compare(other: NAhead) = num.compare(other.num)

  override def toString = numPeriodsAhead + " " + tenor.toString + "s"
}