package starling.pricingschedule

import starling.daterange.Day

/**
 * daysBefore = the number of working days before the event date to price
 * countDaysBefore = the number of counting days between the first pricing period and the event date (X)
 * priceEventDate = the event date itself (0 = event date not priced, 1 = event date priced)
 * daysAfter = the number of non-pricing days between the first pricing period and the event date
 * countDaysAfter = the number of days to price in the second pricing period
 */
case class ExtendedPricingRule(eventDate: Day, priceEventDate: Boolean,
                               daysBefore: Int, countDaysBefore: Int,
                               daysAfter: Int, countDaysAfter: Int,
                               eventDateTreatment: EventDateTreatment)