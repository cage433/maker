package starling.pricingschedule

import starling.calendar.BusinessCalendar
import starling.daterange.Day

abstract class PricingScheduleAlgorithm(nonPricingDayTreatment: NonPricingDayTreatment) {
  def calculateSchedule(calendar: BusinessCalendar): List[Day]

  /**
   * Given a period and the rules for non pricing and pricing days we return a list of pricing day
   * and a multiplier to apply to the value for that day.
   */
  def applyPricingRule(calendar: BusinessCalendar, pricingPeriod: List[Day]): List[(Day, Double)]
}