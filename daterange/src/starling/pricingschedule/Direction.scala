package starling.pricingschedule

import starling.calendar.BusinessCalendar
import starling.daterange.Day

abstract case class Direction(direction: String) {
  def value: Int
}

object Before extends Direction("Before") {
  def value = -1
}
object After extends Direction("After") {
  def value = 1
}

object Direction {
  val types = List(Before, After)
  def parse(s: String) = types.find(_.direction.equalsIgnoreCase(s))
}

