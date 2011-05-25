package starling.pricingschedule

import starling.daterange.{Day, DayOfWeek}
import starling.calendar.BusinessCalendar

abstract class ContinuousPricingScheduleAlgorithmBase(numberOfPricingPeriods: Int, pricingPeriodType: PeriodType, weekStarts: DayOfWeek,
                                                      eventDateTreatment: EventDateTreatment, excludeFirstandLastQuote: Boolean, eventDate: Day,
                                                      nonPricingDayTreatment: NonPricingDayTreatment, countingStart: CountingStartOption, numberOfCountingPeriods: Int,
                                                      countingPeriodType: PeriodType, countDirection: Direction,
                                                      countEventAsDay: Int, includeEventDate: Boolean
                                                       )
  extends FormulatedPricingScheduleAlgorithm(eventDateTreatment, excludeFirstandLastQuote, eventDate, nonPricingDayTreatment) {

  def calculateSchedule(calendar: BusinessCalendar) = {
    val adjustedEventDate = calculateAdjustedEventDate(calendar)
    val countingStartDate = countingStart.applyRule(calendar, eventDate, weekStarts)
    val pricingStartDate = countingPeriodType.pricingStartDay(calendar, countingStartDate, numberOfCountingPeriods, countDirection, countEventAsDay)
    val pricingPeriod = {
      val period = pricingPeriodType.pricingPeriod(calendar, adjustedEventDate, numberOfPricingPeriods, includeEventDate, countingStartDate, pricingStartDate, countDirection)
      applyExcludeFirstandLastQuote(calendar, period)
    }
    pricingPeriod
  }

  def applyPricingRule(calendar: BusinessCalendar, pricingPeriod: List[Day]): List[(Day, Double)] = {
    val adjustedEventDate = calculateAdjustedEventDate(calendar)
    var pricing = List[(Day, Double)]()
    for(day <- pricingPeriod) {
      if(day == adjustedEventDate) {
        if(day.isBusinessDay(calendar)) {
          pricing ::= (day, 1.0)
        } else {
          eventDateTreatment.rule(day) match {
            case Nextx2EventRuleOption => pricing ::= (day.nextBusinessDay(calendar), 1.0)
            case Previousx2EventRuleOption => pricing ::= (day.previousBusinessDay(calendar), 1.0)
            case AverageEventRuleOption => {
              pricing ::= (day.previousBusinessDay(calendar), 0.5)
              pricing ::= (day.nextBusinessDay(calendar), 0.5)
            }
            case _ =>
          }
        }
      } else {
        if(day.isBusinessDay(calendar)) {
          pricing ::= (day, 1.0)
        } else {
          nonPricingDayTreatment.rule(day) match {
            case NextPricingRuleOption => pricing ::= (day.nextBusinessDay(calendar), 1.0)
            case PreviousPricingRuleOption => pricing ::= (day.previousBusinessDay(calendar), 1.0)
            case AveragePricingRuleOption => {
              pricing ::= (day.previousBusinessDay(calendar), 0.5)
              pricing ::= (day.nextBusinessDay(calendar), 0.5)
            }
            case _ =>
          }
        }
      }
    }
    val pricingDaysAndRatios =pricing.groupBy(_._1).map {
      case (day, prices) => {
        val sum = prices.map(_._2).sum
        (day, sum)
      }
    }.toList.sorted

    val total = pricingDaysAndRatios.map(_._2).sum
    pricingDaysAndRatios.map {
      case (day, ratio) => (day, ratio / total)
    }
  }
}
