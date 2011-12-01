package starling.metals.datasources

import collection.immutable.List

import starling.curves.ForwardForwardDiscountCurve
import starling.curves.interestrate.{DayCount, DayCountActual365, DayCount30_360}
import starling.daterange._
import starling.market._
import starling.quantity.{Quantity, UOM}
import starling.utils.ClosureUtil._

import starling.utils.ImplicitConversions._
import scalaz.Scalaz._
import starling.utils.Pattern._
import Day._
import UOM._
import starling.calendar.{BusinessCalendars, BusinessCalendarSet}
import starling.marketdata.ForwardRateSource


case class LIBORFixing(currency: UOM, fixingDay: Day, tenor: Tenor, rate: Quantity) extends Ordered[LIBORFixing] {
  private val dayCountConvention = DayCountActual365//if (currency == UOM.GBP) DayCountActual365 else DayCount30_360
  private val calc = new LIBORCalculator(currency, fixingDay)
  val (valueDay, maturityDay) = (calc.valueDay(tenor), calc.maturityDay(tenor))

  def ccZeroRate: Quantity = (rate * dayCountConvention.factor(valueDay, maturityDay) + 1).ln / dayCountConvention.factor(valueDay, maturityDay)
  def forwardDiscount = 1.0 / (1.0 + 0.01 * rate.value * dayCountConvention.factor(valueDay, maturityDay))//  math.exp(-ccZeroRate.value * dayCountConvention.factor(valueDay, maturityDay))

  def compare(that: LIBORFixing) = {
    require(currency == that.currency, "Cannot compare fixings of different currencies")
    require(fixingDay == that.fixingDay, "Cannot compare fixings of different fixing days")

    maturityDay.compare(that.maturityDay)
  }

  def forwardRate(lastMaturityDay: Day, marketDayAndTime: DayAndTime, forwardForwardRates: Map[DateRange, Quantity]): Quantity = {
    if (valueDay >= lastMaturityDay) {
      rate
    } else {
      assert(maturityDay > lastMaturityDay, "Require maturity days to be strictly increasing")

      val discountCurve = decorate("Cannot build forward forward discount curve for: " + this) {
        new ForwardForwardDiscountCurve(currency, marketDayAndTime, forwardForwardRates)
      }
      val valueDayDiscount = discountCurve.discount(valueDay)
      val lastMaturityDayDiscount = discountCurve.discount(lastMaturityDay)
      val maturityDayDiscount = valueDayDiscount * forwardDiscount
      val forwardRateTime = dayCountConvention.factor(lastMaturityDay, maturityDay)

      val fwdRate = (lastMaturityDayDiscount / maturityDayDiscount - 1.0) / forwardRateTime
//      val ccRate = -math.log(maturityDayDiscount / lastMaturityDayDiscount) / forwardRateTime
//      val fwdRate = (math.exp(-ccRate * forwardRateTime) - 1.0) / forwardRateTime
      Quantity(fwdRate * 100.0, PERCENT)
    }
  }

//  private def timeUsing(dcc: DayCount, from: Day = valueDay) = dcc.factor(from, maturityDay)
}

case class LIBORCalculator(currency: UOM, fixingDay: Day) {
  import LIBORCalculator._
  lazy val combinedCalendar = if (currency == EUR) calendars(currency) else calendars(currency) && calendars(GBP)

  def valueDay(tenor: Tenor = Tenor.ON): Day = (tenor, currency) match {
    case (_, GBP)                  => fixingDay
    case (Tenor.ON, ONCurrency(_)) => fixingDay
    case (_, EUR)                  => fixingDay.addBusinessDays(calendars(currency), 2)
    case _                         => fixingDay.addBusinessDays(calendars(GBP),      2).thisOrNextBusinessDay(combinedCalendar)
  }

  def maturityDay(tenor: Tenor = Tenor.ON): Day = maturityDay(valueDay(tenor), tenor)

  def maturityDay(theValueDay: Day, tenor: Tenor): Day = {
    val maturityDay = tenor + theValueDay

    if (tenor.isOneOf(Tenor.ON, Tenor.SN, Tenor.OneWeek, Tenor.TwoWeeks)) {
      maturityDay.thisOrNextBusinessDay(combinedCalendar)
    } else if (tenor.tenorType == Month && theValueDay == theValueDay.containingMonth.lastDay) {
      maturityDay.containingMonth.lastDay
    } else {
      modifiedFollowingBusinessDay(maturityDay)
    }
  }

  def supportsOvernight = overnightCalendars.contains(currency)
  def isBusinessDay = fixingDay.isBusinessDay(combinedCalendar)

  private val ONCurrency = Extractor.from[UOM](supportsOvernight.option(_))

  private def modifiedFollowingBusinessDay(day: Day): Day = {
    val firstFollowingBusinessDay = day.thisOrNextBusinessDay(combinedCalendar)

    if (firstFollowingBusinessDay.containingMonth != day.containingMonth) {
      firstFollowingBusinessDay.previousBusinessDay(combinedCalendar)
    } else {
      firstFollowingBusinessDay
    }
  }
}

object LIBORCalculator {
  import Tenor._

  val overnightCalendars = Map(
    CAD → BusinessCalendarSet("CAD LIBOR", Location.Unknown, Set(21 Feb 2011, 23 May 2011, 1 Jul 2011, 1 Aug 2011, 5 Sep 2011, 10 Oct 2011, 11 Nov 2011)),
    //// TARGET calendar (Trans-European Automated Real-time Gross settlement Express Transfer)
    EUR → BusinessCalendarSet("EUR LIBOR", Location.Unknown, Set(22 Apr 2011, 25 Apr 2011, 29 Aug 2011, 26 Dec 2011, 27 Dec 2011, 02 Jan 2012)),
    GBP → BusinessCalendarSet("GBP LIBOR", Location.Unknown, Set(3 Jan 2011, 22 Apr 2011, 25 Apr 2011, 29 Apr 2011, 2 May 2011, 30 May 2011, 29 Aug 2011, 26 Dec 2011, 27 Dec 2011, 2 Jan 2012)),
    USD → BusinessCalendarSet("USD LIBOR", Location.Unknown, Set(17 Jan 2011, 21 Feb 2011, 4 Jul 2011, 5 Sep 2011, 10 Oct 2011, 11 Nov 2011, 24 Nov 2011))
  ) ++ Map(CNY → FuturesExchangeFactory.SHFE.calendar.copy(name = "CNY SHIBOR"))

  val spotNextCalendars = Map(
    AUD → BusinessCalendarSet("AUD LIBOR", Location.Unknown, Set(26 Jan 2011, 13 Jun 2011, 1 Aug 2011, 3 Oct 2011)),
    CHF → BusinessCalendarSet("CHF LIBOR", Location.Unknown, Set(2 Jun 2011, 13 Jun 2011, 1 Aug 2011)),
    //DKK → BusinessCalendarSet("DKK LIBOR", Location.Unknown, Set(20 May 2011, 2 Jun 2011, 13 Jun 2011)),
    JPY → BusinessCalendarSet("JPY LIBOR", Location.Unknown, Set(10 Jan 2011, 11 Feb 2011, 21 Mar 2011, 3 May 2011, 4 May 2011, 5 May 2011, 18 Jul 2011, 19 Sep 2011, 23 Sep 2011, 10 Oct 2011, 3 Nov 2011, 23 Nov 2011)),
    NZD → BusinessCalendarSet("NZD LIBOR", Location.Unknown, Set(24 Jan 2011, 31 Jan 2011, 6 Jun 2011, 24 Oct 2011))
    //SEK → BusinessCalendarSet("SEK LIBOR", Location.Unknown, Set(6 Jan 2011, 2 Jun 2011, 24 Jun 2011))
  )

  val currencies = Map(ForwardRateSource.LIBOR ->> (CAD, EUR, GBP, USD, AUD, CHF, JPY, NZD), ForwardRateSource.SHIBOR ->> CNY)

  val calendars = overnightCalendars ++ spotNextCalendars
  def firstTenorFor(currency: UOM) = if (overnightCalendars.contains(currency)) ON else SN

  def tenorsFor(currency: UOM) = firstTenorFor(currency) :: commonTenors
  private val commonTenors = List(OneWeek, TwoWeeks, OneMonth, TwoMonths, ThreeMonths, SixMonths, NineMonths, TwelveMonths)
  val tenors = ON :: SN :: commonTenors
}