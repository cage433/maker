package starling.calendar

import starling.daterange.Month

/**
 * month has to be between 1 and 12 inclusive.
 */
case class BrentMonth(month: Int) {
  assert(month >= 1 && month <= 12, "Invalid month number: " + month)

  override def toString = BrentMonth.name + " " + monthName

  def monthName = Month.months(month - 1)
}

object BrentMonth {
  val name = "Brent"

  private val Parse = """Brent ([A-Z]+)""".r

  def parse(s: String): Option[BrentMonth] = {
    s match {
      case Parse(name) => Month.months.findIndexOf(s => s == name) match {
        case i if i >= 0 => Some(new BrentMonth(i + 1))
        case _ => None
      }
      case _ => {
        None
      }
    }
  }
}