package starling.market

import starling.utils.Log
import starling.daterange.{DateRange, Month, Day}
import starling.calendar.{BusinessCalendar, BusinessCalendars}

trait FuturesExpiryRule {
  /**
   * Last trading day of the future that has delivery in d
   */
  def lastTradingDay(d: DateRange): Day

  /**
   * Expiry day of the option that has delivery in d
   */
  def expiryDay(d: DateRange): Day = throw new Exception("No expiry rule for " + this)

  /**
   * Expiry day for Calendar Spead Option.
   * @param d Would be, for example on a Nymex WTI 1 month CSO with expiration month July10, Month(2010, 7)
   */
  def csoExpiryDay(d: DateRange): Day = throw new Exception("No cso expiry rule for " + this)

  /**
   * Expiry day for Asian Option.
   */
  def asianExpiryDay(d: DateRange): Day = d.lastDay
}

object FuturesExpiryRule {
  val Null = new FuturesExpiryRule {
    def lastTradingDay(d: DateRange) = throw new Exception("Not implemented")
  }
}

class NoExpiryDataException(d: DateRange) extends Exception("No expiry data found for expiry " + d)
class NoOptionExpiryDataException(m: KnownExpiry, d: DateRange, e: Throwable) extends Exception(m + " has no option expiry data for expiry " + d, e)

trait MonthFuturesExpiryRule extends FuturesExpiryRule {
  def lastTradingDay(d: DateRange) = d match {
    case m: Month => lastTradingDayOfMonth(m)
  }

  def lastTradingDayOfMonth(m: Month): Day

  override def expiryDay(d: DateRange) = d match {
    case m: Month => expiryDayOfMonth(m)
  }

  /**
   * Option expiry
   */
  def expiryDayOfMonth(m: Month): Day
}

abstract class FuturesExpiryRules(businessCalendars: BusinessCalendars) {
  def rule(eaiQuoteID: Int): FuturesExpiryRule

  val SHANGHAI = new ShanghaiExpiryRule
  val SHANGHAI_FUEL_OIL = new ShanghaiFuelOilExpiryRule

  val LME = new FuturesExpiryRule {
    val bc = businessCalendars.LME

    def lastTradingDay(d: DateRange) = d match {
      case day: Day => day.addBusinessDays(bc, -1)
    }

    /**
     * LME options expire 1st Wednesday of every month. Upon expiry enter into Futures position prompt
     * 3rd Wednesday of the same month.
     */
    override def expiryDay(d: DateRange) = d match {
      case d: Day => d.containingMonth.firstWednesday
    }

    override def asianExpiryDay(d: DateRange) = {
      // http://www.arsenal.dn.ua/downloads/LME%20Aluminium%20(Alloy).pdf
      // Last trading day: Business day preceding the Declaration Day of the relevant month before 18:00
      // Declaration date/time: Automatic declaration of in-the-money TAPOs at 15.00 on the last business
      //day of the TAPO month
      assert(d.firstDay.month == d.lastDay.month, d + " should be all in the same month")
      d.lastMonth.lastDay.thisOrPreviousBusinessDay(bc).addBusinessDays(bc, -1)
    }
  }


  /**
   * All days are index days except weekends and Baltic public holidays
   * http://www.exchange.imarex.com/membership/imarex-trading-calendar/
   * FFAs and options expire on the last index day.
   * (p227 Shipping Derivatives and Risk Management)
   * From Brad Clark on 29July2010 - So tomorrow (Friday 30th) at about 13:00 london time
   * the final daily avg will be produced and we will know the monthly settlement average.
   */
  val BALTIC = new MonthFuturesExpiryRule {
    val cal = businessCalendars.BALTIC

    def lastTradingDayOfMonth(m: Month) = m.lastDay.thisOrPreviousBusinessDay(cal)
    
    def expiryDayOfMonth(m: Month) = m.lastDay.thisOrPreviousBusinessDay(cal)
  }

  /**
   * Comex expiry
   *
   * http://www.cmegroup.com/trading/metals/
   */
  trait Comex extends MonthFuturesExpiryRule {
    val cal = businessCalendars.COMEX

    // Comex metals futures expire on the third last
    // business day of the month
    def lastTradingDayOfMonth(m: Month): Day = {
      m.lastDay.nextDay.addBusinessDays(cal, -3)
    }
  }

  val COMEX_GOLD = new Comex {
    // Trading terminates on the fourth business day prior to the underlying futures delivery month.
    // If the expiration day falls on a Friday or immediately prior to an Exchange holiday,
    // expiration will occur on the previous business day.
    // http://www.cmegroup.com/trading/metals/base/copper_contractSpecs_options.html
    def expiryDayOfMonth(m: Month) = {
      val exp = m.firstDay.addBusinessDays(cal, -4)
      if(exp.isFriday || exp.nextDay.isHoliday(cal)) {
        exp.previousBusinessDay(cal)
      } else {
        exp
      }
    }
  }
  val COMEX_SILVER = COMEX_GOLD
  val COMEX_HG_COPPER = COMEX_GOLD
  val COMEX_PT_PA = new Comex {
    // Trading terminates on the third Wednesday of the month preceding the option contract month.
    // In the event that such business day precedes an Exchange holiday,
    // the expiration date shall be the preceding business day.
    // http://www.cmegroup.com/trading/metals/precious/platinum_contractSpecs_options.html
    // http://www.cmegroup.com/trading/metals/precious/palladium_contractSpecs_options.html
    def expiryDayOfMonth(m: Month) = {
      val thirdWed = m.thirdWednesday
      if(thirdWed.nextDay.isHoliday(cal)) {
        thirdWed.previousBusinessDay(cal)
      } else {
        thirdWed
      }
    }
  }

  class ShanghaiExpiryRule extends MonthFuturesExpiryRule {
    // The 15th day of the spot month (postponed if legal holidays)
    // http://www.shfe.com.cn/Ehome/contracts.jsp?&subjectpid=9&subjectid=904&startpage=8####
    def lastTradingDayOfMonth(m: Month): Day = {
      (m.firstDay + 15).thisOrNextBusinessDay(businessCalendars.SFS)
    }

    /**
     * I can't find the rule anywhere. Copying LME as it looks like it might be simlar
     */
    def expiryDayOfMonth(m: Month) = m.firstWednesday
  }

  class ShanghaiFuelOilExpiryRule extends MonthFuturesExpiryRule {
    // The last trading day of the month before the spot month (postponed if legal holidays)
    // http://www.shfe.com.cn/Ehome/contracts.jsp?&subjectpid=9&subjectid=904&startpage=7###
    def lastTradingDayOfMonth(m: Month): Day = {
      (m.firstDay - 1).thisOrNextBusinessDay(businessCalendars.SFS)
    }
    def expiryDayOfMonth(m: Month) = throw new Exception("No options")
  }

  /**
   * Tries to use DB for expiry rules but falls back on definition
   */
  def ICE_WTI(bc: BusinessCalendar, futuresExpiryRule: FuturesExpiryRule) = new MonthFuturesExpiryRule {
    def expiryDayOfMonth(m: Month) = try {
      futuresExpiryRule.expiryDay(m)
    } catch {
      case _:NoExpiryDataException => {
        //https://www.theice.com/productguide/ProductDetails.shtml?specId=908
        lastTradingDayOfMonth(m).addBusinessDays(bc, -2)
      }
    }

    def lastTradingDayOfMonth(m: Month): Day = try {
      futuresExpiryRule.lastTradingDay(m)
    } catch {
      case _:NoExpiryDataException => {
        //https://www.theice.com/productguide/ProductDetails.shtml?specId=213
        ((m - 1).firstDay + 24).thisOrPreviousBusinessDay(bc).addBusinessDays(bc, -4)
      }
    }
  }

  /**
   * Platts Dubai or Dubai Crude
   * No idea what the expiry rule is
   * TODO Jerome
   */
  val dubaiCrudeExpiryRule = new MonthFuturesExpiryRule {
    // The last trading day of the month before the spot month (postponed if legal holidays)
    // http://www.shfe.com.cn/Ehome/contracts.jsp?&subjectpid=9&subjectid=904&startpage=7###
    def lastTradingDayOfMonth(m: Month): Day = {
      m.lastDay.thisOrPreviousBusinessDay(businessCalendars.PLD)
    }
    def expiryDayOfMonth(m: Month) = throw new Exception("No options")
  }
  /**
   * Platts Brent
   * No idea what the expiry rule is
   * TODO Jerome
   */
  val plattsBrentExpiryRule = new MonthFuturesExpiryRule {
    // The last trading day of the month before the spot month (postponed if legal holidays)
    // http://www.shfe.com.cn/Ehome/contracts.jsp?&subjectpid=9&subjectid=904&startpage=7###
    def lastTradingDayOfMonth(m: Month): Day = {
      m.lastDay.thisOrPreviousBusinessDay(businessCalendars.PLATTS_EUROPEAN_CRUDE)
    }
    def expiryDayOfMonth(m: Month) = throw new Exception("No options")
  }
}
