package starling.market

import starling.calendar.{HolidayTablesFactory, BusinessCalendars}
import starling.daterange.{TestHolidays, Day, Month, DateRange}
import org.testng.annotations.{BeforeTest, AfterTest, AfterSuite, BeforeSuite}
import org.scalatest.testng.TestNGSuite

trait TestMarketSpec extends ExpiryRulesSpec with TestNGSuite {
  val bla = new TestMarketLookup()

  MarketProvider.registerNewImplForTesting(Some(bla))
}

trait ExpiryRulesSpec extends PrecisionRulesSpec {
  var oldRule: Option[FuturesExpiryRules] = FuturesExpiryRuleFactory.expiryRuleOpton

  FuturesExpiryRuleFactory.registerNewRulesImplForTesting(Some(new FuturesExpiryRules(new BusinessCalendars(HolidayTablesFactory.holidayTables)) {
    val emptyRule = new FuturesExpiryRule {
      val name = "Test"

      def lastTradingDay(d: DateRange) = d.firstDay - 3

      override def expiryDay(d: DateRange) = lastTradingDay(d) - 1

      override def csoExpiryDay(d: DateRange) = lastTradingDay(d) - 2
    }

    def ruleOption(eaiQuoteID: Int) = eaiQuoteID match {
      case 2 => Some(nymex_wti)
      case 14 => Some(ice_gas_oil)
      case _ => Some(emptyRule)
    }
  }))

  @AfterTest
  def after2 {
    FuturesExpiryRuleFactory.registerNewRulesImplForTesting(oldRule)
  }


  // Some hard coded rules:

  val ice_gas_oil = new MonthFuturesExpiryRule{
    val name = "Test"

    def expiryDayOfMonth(m: Month) = oe(m)

    def lastTradingDayOfMonth(m: Month) = ltd(m)

    val ltd = Map(
      Month(2009, 1) -> Day(2009, 1, 12),
      Month(2009, 2) -> Day(2009, 2, 12),
      Month(2009, 3) -> Day(2009, 3, 12),
      Month(2009, 4) -> Day(2009, 4, 8),
      Month(2009, 5) -> Day(2009, 5, 12),
      Month(2009, 6) -> Day(2009, 6, 11),
      Month(2009, 7) -> Day(2009, 7, 10),
      Month(2009, 8) -> Day(2009, 8, 12),
      Month(2009, 9) -> Day(2009, 9, 10),
      Month(2009, 10) -> Day(2009, 10, 12),
      Month(2009, 11) -> Day(2009, 11, 12),
      Month(2009, 12) -> Day(2009, 12, 10),
      Month(2010, 1) -> Day(2010, 1, 12),
      Month(2010, 2) -> Day(2010, 2, 11),
      Month(2010, 3) -> Day(2010, 3, 11),
      Month(2010, 4) -> Day(2010, 4, 12),
      Month(2010, 5) -> Day(2010, 5, 12),
      Month(2010, 6) -> Day(2010, 6, 10),
      Month(2010, 7) -> Day(2010, 7, 12),
      Month(2010, 8) -> Day(2010, 8, 12),
      Month(2010, 9) -> Day(2010, 9, 10),
      Month(2010, 10) -> Day(2010, 10, 12),
      Month(2010, 11) -> Day(2010, 11, 11),
      Month(2010, 12) -> Day(2010, 12, 10),
      Month(2011, 1) -> Day(2011, 1, 12),
      Month(2011, 2) -> Day(2011, 2, 10),
      Month(2011, 3) -> Day(2011, 3, 10),
      Month(2011, 4) -> Day(2011, 4, 12),
      Month(2011, 5) -> Day(2011, 5, 12),
      Month(2011, 6) -> Day(2011, 6, 10),
      Month(2011, 7) -> Day(2011, 7, 12),
      Month(2011, 8) -> Day(2011, 8, 11),
      Month(2011, 9) -> Day(2011, 9, 12),
      Month(2011, 10) -> Day(2011, 10, 12),
      Month(2011, 11) -> Day(2011, 11, 10),
      Month(2011, 12) -> Day(2011, 12, 12),
      Month(2012, 1) -> Day(2012, 1, 12),
      Month(2012, 2) -> Day(2012, 2, 10),
      Month(2012, 3) -> Day(2012, 3, 12),
      Month(2012, 4) -> Day(2012, 4, 12),
      Month(2012, 5) -> Day(2012, 5, 10),
      Month(2012, 6) -> Day(2012, 6, 12),
      Month(2012, 7) -> Day(2012, 7, 12),
      Month(2012, 8) -> Day(2012, 8, 10),
      Month(2012, 9) -> Day(2012, 9, 12),
      Month(2012, 10) -> Day(2012, 10, 11),
      Month(2012, 11) -> Day(2012, 11, 12),
      Month(2012, 12) -> Day(2012, 12, 12),
      Month(2013, 1) -> Day(2013, 1, 10),
      Month(2013, 2) -> Day(2013, 2, 12),
      Month(2013, 3) -> Day(2013, 3, 12),
      Month(2013, 4) -> Day(2013, 4, 11),
      Month(2013, 5) -> Day(2013, 5, 10),
      Month(2013, 6) -> Day(2013, 6, 12),
      Month(2013, 7) -> Day(2013, 7, 11),
      Month(2013, 8) -> Day(2013, 8, 12),
      Month(2013, 9) -> Day(2013, 9, 12),
      Month(2013, 10) -> Day(2013, 10, 10),
      Month(2013, 11) -> Day(2013, 11, 12),
      Month(2013, 12) -> Day(2013, 12, 12),
      Month(2014, 1) -> Day(2014, 1, 10),
      Month(2014, 2) -> Day(2014, 2, 12),
      Month(2014, 3) -> Day(2014, 3, 12),
      Month(2014, 4) -> Day(2014, 4, 10),
      Month(2014, 5) -> Day(2014, 5, 12),
      Month(2014, 6) -> Day(2014, 6, 12),
      Month(2014, 7) -> Day(2014, 7, 10),
      Month(2014, 8) -> Day(2014, 8, 12),
      Month(2014, 9) -> Day(2014, 9, 11),
      Month(2014, 10) -> Day(2014, 10, 10),
      Month(2014, 11) -> Day(2014, 11, 12),
      Month(2014, 12) -> Day(2014, 12, 11),
      Month(2015, 1) -> Day(2015, 1, 12),
      Month(2015, 2) -> Day(2015, 2, 12),
      Month(2015, 3) -> Day(2015, 3, 12),
      Month(2015, 4) -> Day(2015, 4, 10),
      Month(2015, 5) -> Day(2015, 5, 12),
      Month(2015, 6) -> Day(2015, 6, 11),
      Month(2015, 7) -> Day(2015, 7, 10),
      Month(2015, 8) -> Day(2015, 8, 12),
      Month(2015, 9) -> Day(2015, 9, 10),
      Month(2015, 10) -> Day(2015, 10, 12),
      Month(2015, 11) -> Day(2015, 11, 12),
      Month(2015, 12) -> Day(2015, 12, 10)

      )

    val oe = Map(
      Month(2009, 1) -> Day(2009, 1, 5),
      Month(2009, 2) -> Day(2009, 2, 5),
      Month(2009, 3) -> Day(2009, 3, 5),
      Month(2009, 4) -> Day(2009, 4, 1),
      Month(2009, 5) -> Day(2009, 5, 5),
      Month(2009, 6) -> Day(2009, 6, 4),
      Month(2009, 7) -> Day(2009, 7, 3),
      Month(2009, 8) -> Day(2009, 8, 5),
      Month(2009, 9) -> Day(2009, 9, 3),
      Month(2009, 10) -> Day(2009, 10, 5),
      Month(2009, 11) -> Day(2009, 11, 5),
      Month(2009, 12) -> Day(2009, 12, 3),
      Month(2010, 1) -> Day(2010, 1, 5),
      Month(2010, 2) -> Day(2010, 2, 4),
      Month(2010, 3) -> Day(2010, 3, 4),
      Month(2010, 4) -> Day(2010, 4, 1),
      Month(2010, 5) -> Day(2010, 5, 5),
      Month(2010, 6) -> Day(2010, 6, 3),
      Month(2010, 7) -> Day(2010, 7, 5),
      Month(2010, 8) -> Day(2010, 8, 5),
      Month(2010, 9) -> Day(2010, 9, 3),
      Month(2010, 10) -> Day(2010, 10, 5),
      Month(2010, 11) -> Day(2010, 11, 4),
      Month(2010, 12) -> Day(2010, 12, 3),
      Month(2011, 1) -> Day(2011, 1, 5),
      Month(2011, 2) -> Day(2011, 2, 3),
      Month(2011, 3) -> Day(2011, 3, 3),
      Month(2011, 4) -> Day(2011, 4, 5),
      Month(2011, 5) -> Day(2011, 5, 5),
      Month(2011, 6) -> Day(2011, 6, 3),
      Month(2011, 7) -> Day(2011, 7, 5),
      Month(2011, 8) -> Day(2011, 8, 4),
      Month(2011, 9) -> Day(2011, 9, 5),
      Month(2011, 10) -> Day(2011, 10, 5),
      Month(2011, 11) -> Day(2011, 11, 3),
      Month(2011, 12) -> Day(2011, 12, 5),
      Month(2012, 1) -> Day(2012, 1, 5),
      Month(2012, 2) -> Day(2012, 2, 3),
      Month(2012, 3) -> Day(2012, 3, 5),
      Month(2012, 4) -> Day(2012, 4, 3),
      Month(2012, 5) -> Day(2012, 5, 2),
      Month(2012, 6) -> Day(2012, 6, 1),
      Month(2012, 7) -> Day(2012, 7, 5),
      Month(2012, 8) -> Day(2012, 8, 3),
      Month(2012, 9) -> Day(2012, 9, 5),
      Month(2012, 10) -> Day(2012, 10, 4),
      Month(2012, 11) -> Day(2012, 11, 5),
      Month(2012, 12) -> Day(2012, 12, 5),
      Month(2013, 1) -> Day(2013, 1, 3),
      Month(2013, 2) -> Day(2013, 2, 5),
      Month(2013, 3) -> Day(2013, 3, 5),
      Month(2013, 4) -> Day(2013, 4, 4),
      Month(2013, 5) -> Day(2013, 5, 2),
      Month(2013, 6) -> Day(2013, 6, 5),
      Month(2013, 7) -> Day(2013, 7, 4),
      Month(2013, 8) -> Day(2013, 8, 5),
      Month(2013, 9) -> Day(2013, 9, 5),
      Month(2013, 10) -> Day(2013, 10, 3),
      Month(2013, 11) -> Day(2013, 11, 5),
      Month(2013, 12) -> Day(2013, 12, 5),
      Month(2014, 1) -> Day(2014, 1, 3),
      Month(2014, 2) -> Day(2014, 2, 5),
      Month(2014, 3) -> Day(2014, 3, 5),
      Month(2014, 4) -> Day(2014, 4, 3),
      Month(2014, 5) -> Day(2014, 5, 2),
      Month(2014, 6) -> Day(2014, 6, 5),
      Month(2014, 7) -> Day(2014, 7, 3),
      Month(2014, 8) -> Day(2014, 8, 5),
      Month(2014, 9) -> Day(2014, 9, 4),
      Month(2014, 10) -> Day(2014, 10, 3),
      Month(2014, 11) -> Day(2014, 11, 5),
      Month(2014, 12) -> Day(2014, 12, 4),
      Month(2015, 1) -> Day(2015, 1, 5),
      Month(2015, 2) -> Day(2015, 2, 5),
      Month(2015, 3) -> Day(2015, 3, 5),
      Month(2015, 4) -> Day(2015, 4, 1),
      Month(2015, 5) -> Day(2015, 5, 5),
      Month(2015, 6) -> Day(2015, 6, 4),
      Month(2015, 7) -> Day(2015, 7, 3),
      Month(2015, 8) -> Day(2015, 8, 5),
      Month(2015, 9) -> Day(2015, 9, 3),
      Month(2015, 10) -> Day(2015, 10, 5),
      Month(2015, 11) -> Day(2015, 11, 5),
      Month(2015, 12) -> Day(2015, 12, 3)
    )
  }
  // Nymex WTI

  val nymex_wti = new MonthFuturesExpiryRule {
    val name = "Test"

    override def csoExpiryDay(d: DateRange) = oe(d.asInstanceOf[Month]) - 1

    def expiryDayOfMonth(m: Month) = oe(m)

    def lastTradingDayOfMonth(m: Month) = ltd(m)

    val ltd = Map(
      Month(2009, 1) -> Day(2008, 12, 19),
      Month(2009, 2) -> Day(2009, 1, 20),
      Month(2009, 3) -> Day(2009, 2, 20),
      Month(2009, 4) -> Day(2009, 3, 20),
      Month(2009, 5) -> Day(2009, 4, 21),
      Month(2009, 6) -> Day(2009, 5, 19),
      Month(2009, 7) -> Day(2009, 6, 22),
      Month(2009, 8) -> Day(2009, 7, 21),
      Month(2009, 9) -> Day(2009, 8, 20),
      Month(2009, 10) -> Day(2009, 9, 22),
      Month(2009, 11) -> Day(2009, 10, 20),
      Month(2009, 12) -> Day(2009, 11, 20),
      Month(2010, 1) -> Day(2009, 12, 21),
      Month(2010, 2) -> Day(2010, 1, 20),
      Month(2010, 3) -> Day(2010, 2, 22),
      Month(2010, 4) -> Day(2010, 3, 22),
      Month(2010, 5) -> Day(2010, 4, 20),
      Month(2010, 6) -> Day(2010, 5, 20),
      Month(2010, 7) -> Day(2010, 6, 22),
      Month(2010, 8) -> Day(2010, 7, 20),
      Month(2010, 9) -> Day(2010, 8, 20),
      Month(2010, 10) -> Day(2010, 9, 21),
      Month(2010, 11) -> Day(2010, 10, 20),
      Month(2010, 12) -> Day(2010, 11, 19),
      Month(2011, 1) -> Day(2010, 12, 20),
      Month(2011, 2) -> Day(2011, 1, 20),
      Month(2011, 3) -> Day(2011, 2, 22),
      Month(2011, 4) -> Day(2011, 3, 22),
      Month(2011, 5) -> Day(2011, 4, 19),
      Month(2011, 6) -> Day(2011, 5, 20),
      Month(2011, 7) -> Day(2011, 6, 21),
      Month(2011, 8) -> Day(2011, 7, 20),
      Month(2011, 9) -> Day(2011, 8, 22),
      Month(2011, 10) -> Day(2011, 9, 20),
      Month(2011, 11) -> Day(2011, 10, 20),
      Month(2011, 12) -> Day(2011, 11, 18),
      Month(2012, 1) -> Day(2011, 12, 20),
      Month(2012, 2) -> Day(2012, 1, 20),
      Month(2012, 3) -> Day(2012, 2, 21),
      Month(2012, 4) -> Day(2012, 3, 20),
      Month(2012, 5) -> Day(2012, 4, 20),
      Month(2012, 6) -> Day(2012, 5, 22),
      Month(2012, 7) -> Day(2012, 6, 20),
      Month(2012, 8) -> Day(2012, 7, 20),
      Month(2012, 9) -> Day(2012, 8, 21),
      Month(2012, 10) -> Day(2012, 9, 20),
      Month(2012, 11) -> Day(2012, 10, 22),
      Month(2012, 12) -> Day(2012, 11, 16),
      Month(2013, 1) -> Day(2012, 12, 19),
      Month(2013, 2) -> Day(2013, 1, 22),
      Month(2013, 3) -> Day(2013, 2, 20),
      Month(2013, 4) -> Day(2013, 3, 20),
      Month(2013, 5) -> Day(2013, 4, 22),
      Month(2013, 6) -> Day(2013, 5, 21),
      Month(2013, 7) -> Day(2013, 6, 20),
      Month(2013, 8) -> Day(2013, 7, 22),
      Month(2013, 9) -> Day(2013, 8, 20),
      Month(2013, 10) -> Day(2013, 9, 20),
      Month(2013, 11) -> Day(2013, 10, 22),
      Month(2013, 12) -> Day(2013, 11, 20)
      )

    val oe = Map(
      Month(2009, 1) -> Day(2008, 12, 16),
      Month(2009, 2) -> Day(2009, 1, 14),
      Month(2009, 3) -> Day(2009, 2, 17),
      Month(2009, 4) -> Day(2009, 3, 17),
      Month(2009, 5) -> Day(2009, 4, 16),
      Month(2009, 6) -> Day(2009, 5, 14),
      Month(2009, 7) -> Day(2009, 6, 17),
      Month(2009, 8) -> Day(2009, 7, 16),
      Month(2009, 9) -> Day(2009, 8, 17),
      Month(2009, 10) -> Day(2009, 9, 17),
      Month(2009, 11) -> Day(2009, 10, 15),
      Month(2009, 12) -> Day(2009, 11, 17),
      Month(2010, 1) -> Day(2009, 12, 16),
      Month(2010, 2) -> Day(2010, 1, 14),
      Month(2010, 3) -> Day(2010, 2, 17),
      Month(2010, 4) -> Day(2010, 3, 17),
      Month(2010, 5) -> Day(2010, 4, 15),
      Month(2010, 6) -> Day(2010, 5, 17),
      Month(2010, 7) -> Day(2010, 6, 17),
      Month(2010, 8) -> Day(2010, 7, 15),
      Month(2010, 9) -> Day(2010, 8, 17),
      Month(2010, 10) -> Day(2010, 9, 16),
      Month(2010, 11) -> Day(2010, 10, 15),
      Month(2010, 12) -> Day(2010, 11, 16),
      Month(2011, 1) -> Day(2010, 12, 15),
      Month(2011, 2) -> Day(2011, 1, 14),
      Month(2011, 3) -> Day(2011, 2, 16),
      Month(2011, 4) -> Day(2011, 3, 17),
      Month(2011, 5) -> Day(2011, 4, 14),
      Month(2011, 6) -> Day(2011, 5, 17),
      Month(2011, 7) -> Day(2011, 6, 16),
      Month(2011, 8) -> Day(2011, 7, 15),
      Month(2011, 9) -> Day(2011, 8, 17),
      Month(2011, 10) -> Day(2011, 9, 15),
      Month(2011, 11) -> Day(2011, 10, 17),
      Month(2011, 12) -> Day(2011, 11, 15),
      Month(2012, 1) -> Day(2011, 12, 15),
      Month(2012, 2) -> Day(2012, 1, 17),
      Month(2012, 3) -> Day(2012, 2, 15),
      Month(2012, 4) -> Day(2012, 3, 15),
      Month(2012, 5) -> Day(2012, 4, 17),
      Month(2012, 6) -> Day(2012, 5, 17),
      Month(2012, 7) -> Day(2012, 6, 15),
      Month(2012, 8) -> Day(2012, 7, 17),
      Month(2012, 9) -> Day(2012, 8, 16),
      Month(2012, 10) -> Day(2012, 9, 17),
      Month(2012, 11) -> Day(2012, 10, 17),
      Month(2012, 12) -> Day(2012, 11, 13),
      Month(2013, 1) -> Day(2012, 12, 14),
      Month(2013, 2) -> Day(2013, 1, 16),
      Month(2013, 3) -> Day(2013, 2, 14),
      Month(2013, 4) -> Day(2013, 3, 15),
      Month(2013, 5) -> Day(2013, 4, 17),
      Month(2013, 6) -> Day(2013, 5, 16),
      Month(2013, 7) -> Day(2013, 6, 17),
      Month(2013, 8) -> Day(2013, 7, 17),
      Month(2013, 9) -> Day(2013, 8, 15),
      Month(2013, 10) -> Day(2013, 9, 17),
      Month(2013, 11) -> Day(2013, 10, 17),
      Month(2013, 12) -> Day(2013, 11, 15),
      Month(2014, 1) -> Day(2013, 12, 16),
      Month(2014, 2) -> Day(2014, 1, 15),
      Month(2014, 3) -> Day(2014, 2, 14),
      Month(2014, 4) -> Day(2014, 3, 17),
      Month(2014, 5) -> Day(2014, 4, 16),
      Month(2014, 6) -> Day(2014, 5, 15),
      Month(2014, 7) -> Day(2014, 6, 17),
      Month(2014, 8) -> Day(2014, 7, 17),
      Month(2014, 9) -> Day(2014, 8, 15),
      Month(2014, 10) -> Day(2014, 9, 17),
      Month(2014, 11) -> Day(2014, 10, 16),
      Month(2014, 12) -> Day(2014, 11, 17))
  }

}
