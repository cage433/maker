package starling.curves.readers

import collection.immutable.List

import starling.LIMServer
import starling.calendar.BusinessCalendarSet
import starling.daterange._
import starling.db.MarketDataEntry
import starling.market._
import starling.marketdata.{PriceFixingsHistoryData, PriceFixingsHistoryDataKey}
import starling.pivot.MarketValue
import starling.quantity.{Quantity, UOM}

import Day._
import LIMServer._
import UOM._
import starling.utils.ImplicitConversions._
import java.text.DecimalFormat
import starling.utils.Pattern._
import scalaz.Scalaz._


object LIBORFixings extends HierarchicalLimSource(TopRelation.Trafigura.Bloomberg.InterestRates.children, List(Level.Close)) {
  type Relation = LIBORRelation

  case class LIBORRelation(interestRateType: String, currency: UOM, period: StoredFixingPeriod) {
    val group = (interestRateType, currency)
  }

  def relationExtractor = Extractor.regex[Option[LIBORRelation]]("""TRAF\.(\w+)\.(\w+)\.(\w+)""") {
    case List(rateType, UOM.Parse(ccy), Tenor.Parse(tenor)) => Some(LIBORRelation(rateType, ccy, StoredFixingPeriod.tenor(tenor)))
  }

  def marketDataEntriesFrom(fixings: List[Prices[LIBORRelation]]) = {
    fixings.groupBy(group).map { case ((rateType, currency, observationDay), grouped) =>
      MarketDataEntry(observationDay.atTimeOfDay(ObservationTimeOfDay.LiborClose),
        PriceFixingsHistoryDataKey(currency.toString, Some(rateType)),
        PriceFixingsHistoryData.create(grouped.map(fixings => (Level.Close, fixings.relation.period) → marketValue(fixings)))
      )
    }
  }

  def group(fixings: Prices[LIBORRelation]) =
    (fixings.relation.interestRateType, fixings.relation.currency, fixings.observationDay)

  def marketValue(fixings: Prices[LIBORRelation]) = MarketValue.percentage(fixings.priceByLevel(Level.Close) / 100)
}

case class LIBORFixing(value: Quantity, fixingDay: Day) {
  import LIBORFixing._
  val currency = value.uom
  lazy val combinedCalendar = if (currency == EUR) calendars(currency) else calendars(currency) && calendars(GBP)

  def valueDay(tenor: Tenor = Tenor.ON): Day = (tenor, currency) match {
    case (_, GBP)                  => fixingDay
    case (Tenor.ON, ONCurrency(_)) => fixingDay
    case (_, EUR)                  => fixingDay.addBusinessDays(calendars(currency), 2)
    case _                         => fixingDay.addBusinessDays(calendars(GBP),      2).thisOrNextBusinessDay(combinedCalendar)
  }

  def maturityDay(tenor: Tenor = Tenor.ON): Day = maturityDay(valueDay(tenor), tenor)

  def maturityDay(theValueDay: Day, tenor: Tenor): Day = {
    val maturityDay = theValueDay + tenor

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
  def format(format: DecimalFormat) = value.format(format)

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

object LIBORFixing {
  import Tenor._

  val overnightCalendars = Map(
    CAD → BusinessCalendarSet("CAD LIBOR", Location.Unknown, Set(21 Feb 2011, 23 May 2011, 1 Jul 2011, 1 Aug 2011, 5 Sep 2011, 10 Oct 2011, 11 Nov 2011)),
    //// TARGET calendar (Trans-European Automated Real-time Gross settlement Express Transfer)
    EUR → BusinessCalendarSet("EUR LIBOR", Location.Unknown, Set(22 Apr 2011, 25 Apr 2011, 29 Aug 2011, 26 Dec 2011, 27 Dec 2011, 02 Jan 2012)),
    GBP → BusinessCalendarSet("GBP LIBOR", Location.Unknown, Set(3 Jan 2011, 22 Apr 2011, 25 Apr 2011, 29 Apr 2011, 2 May 2011, 30 May 2011, 29 Aug 2011, 26 Dec 2011, 27 Dec 2011, 2 Jan 2012)),
    USD → BusinessCalendarSet("USD LIBOR", Location.Unknown, Set(17 Jan 2011, 21 Feb 2011, 4 Jul 2011, 5 Sep 2011, 10 Oct 2011, 11 Nov 2011, 24 Nov 2011))
  )

  val spotNextCalendars = Map(
    AUD → BusinessCalendarSet("AUD LIBOR", Location.Unknown, Set(26 Jan 2011, 13 Jun 2011, 1 Aug 2011, 3 Oct 2011)),
    CHF → BusinessCalendarSet("CHF LIBOR", Location.Unknown, Set(2 Jun 2011, 13 Jun 2011, 1 Aug 2011)),
    //DKK → BusinessCalendarSet("DKK LIBOR", Location.Unknown, Set(20 May 2011, 2 Jun 2011, 13 Jun 2011)),
    JPY → BusinessCalendarSet("JPY LIBOR", Location.Unknown, Set(10 Jan 2011, 11 Feb 2011, 21 Mar 2011, 3 May 2011, 4 May 2011, 5 May 2011, 18 Jul 2011, 19 Sep 2011, 23 Sep 2011, 10 Oct 2011, 3 Nov 2011, 23 Nov 2011)),
    NZD → BusinessCalendarSet("NZD LIBOR", Location.Unknown, Set(24 Jan 2011, 31 Jan 2011, 6 Jun 2011, 24 Oct 2011))
    //SEK → BusinessCalendarSet("SEK LIBOR", Location.Unknown, Set(6 Jan 2011, 2 Jun 2011, 24 Jun 2011))
  )

  val calendars = overnightCalendars ++ spotNextCalendars
  val currencies = calendars.keySet
  def firstTenorFor(currency: UOM) = if (overnightCalendars.contains(currency)) ON else SN

  def tenorsFor(currency: UOM) = LIBORFixing.firstTenorFor(currency) :: commonTenors
  private val commonTenors = List(OneWeek, TwoWeeks, OneMonth, TwoMonths, ThreeMonths, SixMonths, NineMonths, TwelveMonths)
  val periods = (ON :: SN :: commonTenors).map(tenor => (Level.Close, StoredFixingPeriod.tenor(tenor)))
}