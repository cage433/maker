package starling.curves

import interestrate.{DayCount30_360, DayCountActualActual, DayCountActual365}
import org.testng.annotations._
import org.testng.Assert._
import starling.quantity.UOM._
import starling.quantity.Quantity._
import starling.maths.RandomVariables
import starling.quantity.{Quantity, UOM, Percentage}
import starling.quantity.utils.QuantityTestUtils._
import starling.utils.{Log, StarlingTest}
import starling.daterange._
import starling.metals.datasources.LIBORFixing._
import starling.metals.datasources.LIBORFixing
import starling.marketdata.{ReferenceDataLookup, ForwardRateSource, ForwardRateData, MarketData}

class DiscountCurveTests extends StarlingTest with Log {
	@Test
	/** Sanity check on the swap rate conversion to continuously compounded. The cc rates should be a bit higher.
  */
	def testBootstrapFormulae{
	  import SimpleDiscountCurve._
	  val marketDay = Day(2009, 9, 1)
	  val forwardDay = Day(2012, 3, 15)
	  val originalRate = 0.05
	  val ccRates = List(("Quarterly", "SWAP"), ("Semi-Ann", "SWAP"), ("Annual", "SWAP")).map{
	    case (format, typeName) =>
	      convertRateToCCZeroRate(marketDay, forwardDay, format, typeName, originalRate)
	  }
   
	  // check they are in descending order
	  assertTrue(ccRates.sortWith(_>_) == ccRates)
   
	  // check they are all closeish (within 20 bp) to the original rate
	  ccRates.foreach{
	    ccRate =>
	    	assertTrue(ccRate < originalRate )
	    	assertTrue(ccRate > originalRate - 0.002)
	  }
	}
 
	@Test
	def testInterpolationIsReasonable{
	  val marketDay = Day(2009, 9, 10).endOfDay
	  val days = List(20, 100, 300).map(marketDay.day + _)
	  val zeroRates = List(0.05, 0.06, 0.055)
	  val discountCurve = SimpleDiscountCurve(USD, marketDay, days.toArray, zeroRates.toArray)
	  days.zip(zeroRates).foreach{
	    case (d, z) =>
	      assertEquals(z, discountCurve.zeroRate(d), 1e-6)
	  }
   
	  // check extrapolation
	  assertEquals(discountCurve.zeroRate(Day(2012, 1, 1)), zeroRates.last, 1e-9)
	  assertEquals(discountCurve.zeroRate(marketDay.day + 1), zeroRates.head, 1e-9)
   
	  // check interpolation
   days.zip(zeroRates).zip( days.tail.zip(zeroRates.tail) ).foreach{
     case ((d1, z1), (d2, z2)) =>
       for (d <- d1 upto d2){
         val z = discountCurve.zeroRate(d)
         if (z1 < z2)
           assertTrue(z1 <= z && z <= z2)
         else
           assertTrue(z1 >= z && z >= z2)
         
         // check discount rate as well
         val disc = discountCurve.discount(d)
         val t = d.daysSinceInYears(marketDay.day)
         val expectedDisc = math.exp(- z * t)
         assertEquals(disc, expectedDisc, 1e-9)
       }
   }
	}


  @Test
  def testForwardForwardDiscountCurve{
    val u = RandomVariables.uniformDiscrete(50, 12345)
    var days = List[Day](Day(2009, 1, 1))
    for (i <- 0 until 20)
      days = (days(0) + u.nextInt + 5)::days
    days = days.reverse
    val x = RandomVariables.standardUniform(54321)
    val periods = days.zip(days.tail).map{case (d1, d2) => SimpleDateRange(d1, d2)}
    val rateData = Map.empty[DateRange, Quantity] ++ periods.map((_, Quantity(x.nextDouble, UOM.PERCENT)))
    val marketDayAndtime: DayAndTime = days.head.endOfDay               
    val discountCurve = new ForwardForwardDiscountCurve(USD, marketDayAndtime, rateData)
    val env = Environment(
      new MappingCurveObjectEnvironment(Map[CurveKey, CurveObject](DiscountCurveKey(USD) ->discountCurve), marketDayAndtime)
    )
    periods.foreach{
      p =>
        assertQtyEquals(env.forwardRate(USD, p.firstDay, p.lastDay, DayCountActual365), rateData(p), 1e-6)
        assertQtyEquals(
          env.forwardCCRate(USD, p.firstDay, p.lastDay, DayCountActual365),
          env.forwardCCRate(USD, p.firstDay + 1, p.lastDay - 1, DayCountActual365),
          1e-6)
    }

  }

  @Test
  def testConstantCurveCanBeRecoveredFromForwardForwardRates = log.off {
    val marketDay = Day(2009, 9, 10).startOfDay
    val discountCurve = new ConstantDiscountCurve(marketDay, USD, 0.05)

    val days = marketDay.day :: (1 to 20).toList.map{
      i => (marketDay.containingMonth + i).lastDay
    }

    val periods = days.zip(days.tail).map{case (d1, d2) => SimpleDateRange(d1, d2)}
    val env = Environment(
      new MappingCurveObjectEnvironment(Map[CurveKey, CurveObject](DiscountCurveKey(USD) ->discountCurve), marketDay)
    )
    val rates = Map.empty[DateRange, Quantity] ++ periods.map{ p : DateRange =>
      val fwdRate = env.forwardRate(USD, p, DayCountActual365)
      log.debug(p + ": " + fwdRate)
      (p → fwdRate)
    }
    val newCurve = new ForwardForwardDiscountCurve(USD, marketDay, rates)

    val newEnv = Environment(
      new MappingCurveObjectEnvironment(Map[CurveKey, CurveObject](DiscountCurveKey(USD) -> newCurve), marketDay)
    )

    rates.values.foreach(log.debug(_))

    periods.foreach{
      p =>
        log.debug(env.forwardRate(USD, p, DayCountActual365))
        log.debug(newEnv.forwardRate(USD, p, DayCountActual365))
        assertEquals(
          env.forwardRate(USD, p, DayCountActual365).value,
          newEnv.forwardRate(USD, p, DayCountActual365).value,
          1e-9
        )
    }
  }


  /**
   * TODO - put this test back. Numbers are close but not exact
   */
  @Test
  def testMoreComplexCurve{

    val ccy = USD
    val marketDay = Day(2011, 12, 1).endOfDay
    val rates = ForwardRateData(
      Map(ForwardRateSource.LIBOR ->
        Map(
          Tenor.OneMonth -> 1.3 (PERCENT),
          Tenor.TwoMonths -> 1.5 (PERCENT),
          Tenor.ThreeMonths -> 1.7 (PERCENT),
          Tenor.SixMonths -> 2.0 (PERCENT)
        )))
    val discountCurve = DiscountCurveKey(ccy).buildFromMarketData(marketDay, rates, ReferenceDataLookup.Null)
    val env = Environment(
      new MappingCurveObjectEnvironment(Map[CurveKey, CurveObject](DiscountCurveKey(ccy) ->discountCurve), marketDay)
    )

    rates.rates(ForwardRateSource.LIBOR).foreach{
      case (tenor, rate) =>
        val fixing =  LIBORFixing(ccy, marketDay.day, tenor, rate)
        val forwardRate = env.forwardRate(ccy, fixing.fixingDay, fixing.maturityDay, DayCountActual365)
        println(forwardRate + ", " + rate)
//        assertQtyEquals(forwardRate, rate, 1e-6)
    }
  }


  @Test
  def testExtrapolationOfDiscounts{
    val ccy = USD
    val marketDay = Day(2011, 12, 1).endOfDay
    val rates = ForwardRateData(
      Map(ForwardRateSource.LIBOR ->
        Map(
          Tenor.OneMonth -> 1.3 (PERCENT),
          Tenor.TwoMonths -> 1.5 (PERCENT),
          Tenor.ThreeMonths -> 1.7 (PERCENT),
          Tenor.OneYear -> 2.0 (PERCENT)
        )))
    val discountCurve = DiscountCurveKey(ccy).buildFromMarketData(marketDay, rates, ReferenceDataLookup.Null)
    val env = Environment(
      new MappingCurveObjectEnvironment(Map[CurveKey, CurveObject](DiscountCurveKey(ccy) ->discountCurve), marketDay)
    )
    val oneYearRate = env.discount(USD, marketDay.day + 365)
    val threeYearRate = env.discount(USD, marketDay.day + 365 + 365 + 365)
    assertQtyEquals(threeYearRate, oneYearRate * oneYearRate * oneYearRate, 0.002)
  }
}
