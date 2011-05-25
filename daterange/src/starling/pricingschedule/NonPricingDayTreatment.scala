package starling.pricingschedule

import starling.daterange.{DayOfWeek, Day}

case class PricingRuleOption(rule: String)

object IgnorePricingRuleOption extends PricingRuleOption("Ignore")
object NextPricingRuleOption extends PricingRuleOption("Next")
object PreviousPricingRuleOption extends PricingRuleOption("Previous")
object AveragePricingRuleOption extends PricingRuleOption("Average")

object PricingRuleOption {
  val rules = List(IgnorePricingRuleOption, NextPricingRuleOption, PreviousPricingRuleOption, AveragePricingRuleOption)

  def unapply(a: Any): Option[PricingRuleOption] = rules.find(_.rule.equalsIgnoreCase(a.toString))
}

case class NonPricingDayTreatment(friday: PricingRuleOption = IgnorePricingRuleOption,
                                  saturday: PricingRuleOption = IgnorePricingRuleOption,
                                  sunday: PricingRuleOption = IgnorePricingRuleOption,
                                  monday: PricingRuleOption = IgnorePricingRuleOption,
                                  other: PricingRuleOption = IgnorePricingRuleOption) {
  def rule(day: Day) = day.dayOfWeek match {
    case DayOfWeek.monday => monday
    case DayOfWeek.friday => friday
    case DayOfWeek.saturday => saturday
    case DayOfWeek.sunday => sunday
    case _ => other
  }
}
