package starling.pricingschedule

import starling.daterange.Day

/**
 * priceEventDate - is the event date itself (should/shouldn't be priced)
 * daysAfter - is the number of working days after the event date to price
 * daysBefore - is the number of working days before the event date to price
 */
case class PricingRule(eventDate: Day, priceEventDate: Boolean, daysBefore: Int, daysAfter: Int, eventDateTreatment: EventDateTreatment)