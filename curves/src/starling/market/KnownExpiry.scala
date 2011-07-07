package starling.market

import starling.utils.cache.CacheFactory
import starling.daterange._
import starling.calendar.BusinessCalendar

trait KnownExpiry {
  @transient val expiryRule: FuturesExpiryRule
  @transient val tenor: TenorType
  @transient val hasOptions: Boolean
  @transient val businessCalendar: BusinessCalendar

  def lastTradingDay(d: DateRange) = expiryRule.lastTradingDay(d)

  /**
   * Standard American/European option expiry
   */
  def optionExpiry(delivery: DateRange): Day = {
    try {
      expiryRule.expiryDay(delivery)
    } catch {
      case e => {
        throw new NoOptionExpiryDataException(this, delivery, e)
      }
    }
  }

  /**
   * Calendar spread option expiry
   */
  def csoOptionExpiry(delivery: DateRange): Day = {
    try {
      expiryRule.csoExpiryDay(delivery)
    }
    catch {
      case e => throw new Exception("Failed to find CSO expiry with delivery " + delivery + " on " + this, e)
    }
  }

  /**
   * Calendar spread option expiry
   */
  def csoOptionExpiry(spread: Spread[_ <: DateRange]): Day = {
    try {
      expiryRule.csoExpiryDay(spread.first)
    }
    catch {
      case e => throw new Exception("Failed to find CSO expiry with delivery " + spread + " on " + this, e)
    }
  }

  /**
   * Calendar spread option expiry
   */
  def asianOptionExpiry(delivery: DateRange): Day = {
    try {
      expiryRule.asianExpiryDay(delivery)
    }
    catch {
      case e => throw new Exception("Failed to find CSO expiry with delivery " + delivery + " on " + this, e)
    }
  }

  def validOptionExpiry(expiryDay: Day, lastTradingDay: Day) = {
    if (hasOptions) {
      val front = frontPeriod(lastTradingDay)
      val correctExpiryDay = optionExpiry(front)
      correctExpiryDay == expiryDay
    } else {
      false
    }
  }

  /**
   * Returns the front delivery month/day for a given last trading day.
   */
  private var frontPeriodCache = CacheFactory.getCache("FuturesMarket.frontPeriod", unique = true)

  def frontPeriod(dayAndTime: DayAndTime): DateRange = frontPeriod(dayAndTime.day)
  def frontPeriod(day: Day): DateRange = frontPeriodCache.memoize(
    (day),
    (tuple: (Day)) => {
      tenor match {
        case Month => frontMonth(day.startOfDay)
        case Day => day.nextBusinessDay(businessCalendar)
      }
    })

  @transient private var frontOptionPeriodCache = CacheFactory.getCache("FuturesMarket.frontOptionPeriod", unique = true)

  def frontOptionPeriod(day: Day): DateRange = frontOptionPeriodCache.memoize(
    (day),
    (tuple: (Day)) => tenor match {
    // easy to do for months, not so easy for days. daily markets will override this themselves.
      case Month => {
        var period = frontPeriod(day)
        while (optionExpiry(period) < day) {
          period = period match {
            case m: Month => m + 1
          }
        }
        period
      }
    })

  def validLastTradingDay(lastTradingDay: Day): Boolean = lastTradingDay == this.lastTradingDay(frontPeriod(lastTradingDay))

  /**
   * Return the earliest tradeable month, using the assumption that a contract can be traded
   * at the start of the last trading day, but not the end
   */
  def frontMonth(dayAndTime: DayAndTime): Month = {
    assert(tenor == Month)
    var month = dayAndTime.containingMonth
    try {
      while (expiryRule.lastTradingDay(month).endOfDay <= dayAndTime) {
        month = month + 1
      }
    } catch {
      case e: NoExpiryDataException => throw new Exception("Problem getting front month for " + this, e)
    }
    month
  }

  def nthPeriod(dayAndTime: DayAndTime, numPeriodsAhead: Int): DateRange = tenor match {
    case Month => frontMonth(dayAndTime) + numPeriodsAhead
    case Day => frontPeriod(dayAndTime.day).asInstanceOf[Day] + numPeriodsAhead
  }
}