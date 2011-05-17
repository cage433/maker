package starling.pricingschedule

import starling.daterange.{Day, DayOfWeek}

case class ContinousPricingScheduleAlogorithm(numberOfPricingPeriods: Int,
                                              pricingPeriodType: PeriodType,
                                              weekStarts: DayOfWeek,
                                              eventDateTreatment: EventDateTreatment,
                                              excludeFirstandLastQuote: Boolean,
                                              eventDate: Day,
                                              nonPricingDayTreatment: NonPricingDayTreatment,
                                              // start
                                              countingStart: CountingStartOption,
                                              countEventAsDay: Int,
                                              //counting
                                              numberOfCountingPeriods: Int,
                                              countingPeriodType: PeriodType,
                                              countDirection: Direction,
                                              //pricing
                                              pricingDirection: Direction,
                                              // options
                                              includeEventDate: Boolean,
                                              applyEventDateTreatmentToFirstPricingDay: Boolean,
                                              pricingPerCalendarPeriodOption: PricingPerCalendarPeriodOption)
  extends ContinuousPricingScheduleAlgorithmBase(numberOfPricingPeriods, pricingPeriodType, weekStarts, eventDateTreatment,
    excludeFirstandLastQuote, eventDate, nonPricingDayTreatment, countingStart, numberOfCountingPeriods, countingPeriodType, countDirection, countEventAsDay, includeEventDate)
