package starling.pricingschedule

import starling.daterange.{Day, DayOfWeek}

case class AroundPricingScheduleAlgorithm(numberOfPricingPeriods: Int,
                                          pricingPeriodType: PeriodType,
                                          weekStarts: DayOfWeek,
                                          eventDateTreatment: EventDateTreatment,
                                          eventDate: Day,
                                          nonPricingDayTreatment: NonPricingDayTreatment)
  extends ContinuousPricingScheduleAlgorithmBase(
    numberOfPricingPeriods,
    pricingPeriodType,
    weekStarts,
    eventDateTreatment,
    excludeFirstandLastQuote = false,
    eventDate,
    nonPricingDayTreatment,
    countingStart = DayCountingStartOption,
    numberOfPricingPeriods / 2,
    pricingPeriodType,
    countDirection = Before,
    0,
    (numberOfPricingPeriods % 2) != 0)
