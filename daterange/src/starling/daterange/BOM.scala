package starling.daterange

import java.lang.String
import util.matching.Regex

/**
 * Balance of Month.
 *
 * From 'day' to end of month inclusive
 */
case class BOM(day: Day) extends DateRange {
  def month = day.containingMonth
  def asDateRange = DateRange(firstDay, lastDay)

  override def firstDay = day
  override def lastDay = month.lastDay

  override def compare(that : DateRange) = that match {
    case other: BOM => asDateRange.compare(other.asDateRange)
    case _ => super.compare(that)
  }
  
  override def toString = {
    "BOM(" + firstDay + ")"
  }

  def tenor = Some(BOM)

  def toListOfMonths = throw new Exception("BOM " + this + "Can't be broken into months")

  def +(i : Int) : BOM = {
    copy(day = day + i)
  }

}

object BOM extends TenorType {
  type T = BOM

  // hack to allow Ordered covariance. see comment in Day's companion object.
  implicit def orderedHack[BOM <: DateRange with Ordered[DateRange]](hm : BOM) : Ordered[BOM] = hm.asInstanceOf[Ordered[BOM]]

  def containing(d : Day) : BOM = {
    new BOM(d)
  }

  def add(n: Int, from: BOM) = from + n
  def difference(to: BOM, from: BOM) = to.day - from.day

  val ParserRegex = """BOM\((.+)\)""".r

  def parse(text : String) = {
    text.toUpperCase match {
      case ParserRegex(Day(d)) => BOM(d)
      case _ => throw new IllegalStateException("Can't parse BOM from `" + text + "'")
    }
  }

  override def toString = "BOM"

  def shortName = "BOM"
}
