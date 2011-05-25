package starling.pricingschedule

import starling.daterange.Day
import starling.calendar.BusinessCalendar

abstract class FormulatedPricingScheduleAlgorithm(eventDateTreatment: EventDateTreatment, excludeFirstandLastQuote: Boolean, eventDate: Day,
                                                  nonPricingDayTreatment: NonPricingDayTreatment)
  extends PricingScheduleAlgorithm(nonPricingDayTreatment) {

  def calculateAdjustedEventDate(calendar: BusinessCalendar): Day = {
    if (!calendar.isBusinessDay(eventDate)) {
      eventDateTreatment.nonBusinessEventDate(calendar, eventDate)
    } else {
      eventDate
    }
  }

  /**
   * Apply excludeFirstandLastQuote
   */
  def applyExcludeFirstandLastQuote(calendar: BusinessCalendar, pricingPeriod: List[Day]): List[Day] = {
    if (excludeFirstandLastQuote) {
      pricingPeriod match {
        case Nil => Nil
        case first :: Nil => Nil
        case first :: last => last.take(last.size - 1).toList
      }
    } else {
      pricingPeriod
    }
  }
}