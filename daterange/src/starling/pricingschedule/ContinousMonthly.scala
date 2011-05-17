package starling.pricingschedule

import starling.daterange.Day


case class ContinuousMonthlyPricingEndpoint(dayOfMonth: Int, countMonths: Int, direction: Direction, eventDate: Day)

class ContinousMonthly(startPricing: ContinuousMonthlyPricingEndpoint,
                       endPricing: ContinuousMonthlyPricingEndpoint,
                       includeEventDate: Boolean)