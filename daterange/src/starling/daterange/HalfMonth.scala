package starling.daterange

import java.lang.String
import util.matching.Regex

/**
 * half a calendar month.
 *
 * can be either the front half or the back half, the division between them is the 15th is in the first
 * half of the month, the 16th is in the second.
 */
case class HalfMonth(month : Month, backHalf : Boolean) extends DateRange {
  def this(y : Int, m : Int, fb : FrontOrBack.Value) = this(new Month(y, m), fb != FrontOrBack.Front)

  override def firstDay = month.firstDay + (if (backHalf) (HalfMonth.BACK_FIRST_DAY - 1) else 0)
  override def lastDay = if (backHalf) month.lastDay else month.firstDay + (HalfMonth.BACK_FIRST_DAY - 2)
  override def compare(that : DateRange) = that match {
    case thatHM : HalfMonth => {
      val compareMonth = month compare thatHM.month
      if (compareMonth == 0) backHalf compare thatHM.backHalf else compareMonth
    }
    case _ => super.compare(that)
  }
  
  override def toString = {
    val fbAsString = if (backHalf) " BACK" else " FRONT"
    month.toString + fbAsString
  }

  def intervalNumber = (month.m - 1) * 2 + (if (backHalf) 1 else 0)

  def tenor = Some(HalfMonth)

  def +(i : Int) : HalfMonth = {
    val increment = if (backHalf) i + 1 else i
    new HalfMonth(month + increment / 2, (increment % 2) == 1)
  }
  def -(that : HalfMonth) : Int = {
    val correction = (if (backHalf) 1 else 0) - (if (that.backHalf) 1 else 0)
    2 * (month - that.month) + correction
  }

  def toListOfMonths = throw new Exception("Half month " + this + "Can't be broken into months")

}

object HalfMonth extends TenorType {
  type T = HalfMonth

  // the day number of the first day of the new half-month
  val BACK_FIRST_DAY = 16

  // hack to allow Ordered covariance. see comment in Day's companion object.
  implicit def orderedHack[HalfMonth <: DateRange with Ordered[DateRange]](hm : HalfMonth) : Ordered[HalfMonth] = hm.asInstanceOf[Ordered[HalfMonth]]

  // half-month which contains the day.
  def containing(d : Day) : HalfMonth = {
    new HalfMonth(d.containingMonth, d.dayNumber >= BACK_FIRST_DAY)
  }

  def add(n: Int, from: HalfMonth) = from + n
  def difference(to: HalfMonth, from: HalfMonth) = to - from

  val parserRegex = new Regex("(" + Month.monthRegex.pattern + ") (BACK|FRONT)")

  def parse(text : String) = {
    text.toUpperCase match {
      // the regex class will output the captured groups for the month regex as well. this is
      // kind of annoying, since it means we can't treat month's parser as a black box.
      case parserRegex(monthString, dummyA, dummyB, backFront) =>
        HalfMonth(Month.parse(monthString), backFront == "BACK")
      case _ => throw new IllegalStateException("Can't parse half-month from `" + text + "'")
    }
  }

  override def toString = "HalfMonth"

  def shortName = "HM"
}

object FrontOrBack extends Enumeration {
  val Front = Value("Front")
  val Back = Value("Back")
}
