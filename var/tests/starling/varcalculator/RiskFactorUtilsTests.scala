package starling.varcalculator

import starling.utils.StarlingTest
import org.testng.annotations.Test
import starling.quantity.UOM._
import org.testng.Assert.assertEquals
import starling.market.{TestExpiryRules, FuturesMarket, Market}
import starling.daterange.{DateRange, Month, Day}

class RiskFactorUtilsTests extends TestExpiryRules {
  
  @Test
  def testMonthlyBucketingOfPriceRiskFactors{
    val market = FuturesMarket.testMarket("Lead", USD, MT)
    val marketDay: Day = Day(2009, 10, 10)
    val marketDayAndTime = marketDay.endOfDay

    def makeRiskFactor(start : Day, end : Day) = {
      val fp = market.frontPeriod(marketDay).asInstanceOf[Day]
      ForwardPriceRiskFactor(market, (start - fp).max(0), end - fp)
    }
    def riskFactorDays(rf : ForwardPriceRiskFactor) = {
      val fp = market.frontPeriod(marketDay).asInstanceOf[Day]
      (
      fp + rf.nPeriodsToStart,
      fp + rf.nPeriodsToEnd
    )}
    def buckets(start : Day, end : Day) = {
      RiskFactorUtils.bucketRiskFactors(
        marketDayAndTime, Set(makeRiskFactor(start, end))
      ).map{rf => riskFactorDays(rf.asInstanceOf[ForwardPriceRiskFactor])}
    }

    assertEquals(
      buckets(Day(2009, 10, 10), Day(2009, 10, 30)),
      Set((Day(2009, 10, 12), Day(2009, 10, 31)))
    )

    assertEquals(
      buckets(Day(2009, 10, 10), Day(2009, 11, 15)),
      Set(
        (Day(2009, 10, 12), Day(2009, 10, 31)),
        (Day(2009, 11, 1), Day(2009, 11, 30))
      )
    )
  }

//  @Test
//  def testMonthlyBucketingOfVolRiskFactors{
//    import RiskFactorUtils._
//    val market = Market.LME_LEAD
//    val marketDay: Day = Day(2009, 10, 10)
//    val marketDayAndTime = marketDay.endOfDay
//
//    val may = Month(2010, 5)
//    val vrf1 = VolatilityRiskFactor(market, may)
//    assertEquals(Set(vrf1), bucketRiskFactors(marketDayAndTime, Set(vrf1)))
//
//    val jun = Month(2010, 6)
//    val vrf2 = VolatilityRiskFactor(market, jun)
//    assertEquals(Set(vrf1, vrf2), bucketRiskFactors(marketDayAndTime, Set(vrf1, vrf2)))
//
//    val vrf3:Set[RiskFactor] = may.days.map(VolatilityRiskFactor(market, _)).toSet
//    assertEquals(Set(vrf1), bucketRiskFactors(marketDayAndTime, vrf3))
//
//    val vrf4:Set[RiskFactor] = DateRange(may.firstDay, jun.firstDay).days.map(VolatilityRiskFactor(market, _)).toSet
//    assertEquals(Set(vrf1, vrf2), bucketRiskFactors(marketDayAndTime, vrf4))
//
//    val vrf5:Set[RiskFactor] = DateRange(may.firstDay, jun.firstDay).days.map(VolatilityRiskFactor(market, _)).toSet
//    assertEquals(Set(vrf1, vrf2), bucketRiskFactors(Day(2010, 5, 13).endOfDay, vrf5))
//
//    val may12th = Day(2010, 5, 12)
//    val vrf6:Set[RiskFactor] = DateRange(may12th, jun.firstDay).days.map(VolatilityRiskFactor(market, _)).toSet
//    assertEquals(Set(VolatilityRiskFactor(market, DateRange(may12th, may.lastDay)), vrf2), bucketRiskFactors(marketDayAndTime, vrf6))
//  }

  @Test
  def testLMEBucket {
    val mkt = Market.LME_ALUMINIUM
    val marketDay = Day(2009, 1, 1)
    val rfs:Set[RiskFactor] = Set(ForwardPriceRiskFactor(mkt,1053,1053), ForwardPriceRiskFactor(mkt,1034,1034), ForwardPriceRiskFactor(mkt,1035,1035),
      ForwardPriceRiskFactor(mkt,1055,1055), ForwardPriceRiskFactor(mkt,1060,1060), ForwardPriceRiskFactor(mkt,1047,1047),
      ForwardPriceRiskFactor(mkt,1043,1043), ForwardPriceRiskFactor(mkt,1057,1057), ForwardPriceRiskFactor(mkt,1041,1041),
      ForwardPriceRiskFactor(mkt,1046,1046), ForwardPriceRiskFactor(mkt,1056,1056), ForwardPriceRiskFactor(mkt,1042,1042),
      VolatilityRiskFactor(mkt, Month(2011,11).thirdWednesday), ForwardPriceRiskFactor(mkt,1054,1054), ForwardPriceRiskFactor(mkt,1049,1049),
      VolatilityRiskFactor(mkt, Month(2011,12).thirdWednesday), ForwardPriceRiskFactor(mkt,1050,1050), ForwardPriceRiskFactor(mkt,1048,1048),
      ForwardPriceRiskFactor(mkt,1039,1039), ForwardPriceRiskFactor(mkt,1061,1061), ForwardPriceRiskFactor(mkt,1040,1040), ForwardPriceRiskFactor(mkt,1036,1036))

    val bucketed = RiskFactorUtils.bucketRiskFactors(marketDay.endOfDay, rfs)

    assertEquals(Set(ForwardPriceRiskFactor(mkt,1030,1059), // nov
    ForwardPriceRiskFactor(mkt,1060,1090), // dec
      VolatilityRiskFactor(mkt, Month(2011, 11).thirdWednesday),
      VolatilityRiskFactor(mkt, Month(2011, 12).thirdWednesday)), bucketed)
  }

  @Test
  def testVolRiskFactors {
    val market = Market.NYMEX_WTI
    val march = Month(2010, 3)
    val apr = Month(2010, 4)
    val rfs: Set[RiskFactor] = Set(ForwardPriceRiskFactor(market, 1, 2), VolatilityRiskFactor(market, march), VolatilityRiskFactor(market, apr))
    val rfsNoMarch: Set[RiskFactor] = Set(ForwardPriceRiskFactor(market, 1, 2), VolatilityRiskFactor(market, apr))

    assertEquals(VolatilityRiskFactor(market, march), RiskFactorUtils.volRiskFactor(march, rfs))
    assertEquals(VolatilityRiskFactor(market, apr), RiskFactorUtils.volRiskFactor(apr, rfs))

    assertEquals(VolatilityRiskFactor(market, apr), RiskFactorUtils.volRiskFactor(march, rfsNoMarch))
    assertEquals(VolatilityRiskFactor(market, apr), RiskFactorUtils.volRiskFactor(apr, rfsNoMarch))
  }
}
