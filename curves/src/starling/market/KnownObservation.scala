package starling.market

import starling.utils.cache.CacheFactory
import starling.daterange.{Day, DateRange}

trait KnownObservation {
  @transient private lazy val observationDayCache = CacheFactory.getCache(this.toString, unique = true)

  def observationDays(period : DateRange) : List[Day] = observationDayCache.memoize(period, period.days.filter(isObservationDay).toList)

  /** Whether or not market prices observed on this day
   */
  def isObservationDay(day : Day): Boolean
}