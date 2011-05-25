package starling.pricingschedule

import starling.daterange.{DayOfWeek, Day}
import starling.calendar.BusinessCalendar

case class EventRuleOption(rule: String)

object IgnoreEventRuleOption extends EventRuleOption("Ignore")
object NextEventRuleOption extends EventRuleOption("Next")
object PreviousEventRuleOption extends EventRuleOption("Previous")
object AverageEventRuleOption extends EventRuleOption("Average")
object Nextx2EventRuleOption extends EventRuleOption("Nextx2")
object Previousx2EventRuleOption extends EventRuleOption("Previousx2")

object EventRuleOption {
  val rules = List(IgnoreEventRuleOption, NextEventRuleOption, PreviousEventRuleOption, AverageEventRuleOption, Nextx2EventRuleOption, Previousx2EventRuleOption)

  def unapply(a: Any): Option[EventRuleOption] = rules.find(_.rule.equalsIgnoreCase(a.toString))
}

case class EventDateTreatment(friday: EventRuleOption = IgnoreEventRuleOption,
                              saturday: EventRuleOption = IgnoreEventRuleOption,
                              sunday: EventRuleOption = IgnoreEventRuleOption,
                              monday: EventRuleOption = IgnoreEventRuleOption,
                              other: EventRuleOption = IgnoreEventRuleOption) {

  def nonBusinessEventDate(calendar: BusinessCalendar, day: Day) = rule(day) match {
    case NextEventRuleOption => calendar.nextBusinessDay(day)
    case PreviousEventRuleOption => calendar.previousBusinessDay(day)
  }

  def rule(day: Day) = day.dayOfWeek match {
    case DayOfWeek.monday => monday
    case DayOfWeek.friday => friday
    case DayOfWeek.saturday => saturday
    case DayOfWeek.sunday => sunday
    case _ => other
  }
}