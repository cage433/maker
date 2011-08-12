package starling.instrument

import starling.utils.StarlingTest
import org.testng.annotations.Test
import starling.quantity.UOM._
import starling.market.Market._
import org.testng.Assert._
import starling.utils.QuantityTestUtils._
import starling.curves._
import starling.market._
import starling.daterange.{TestHolidays, DayAndTime, Day, Month}
import starling.quantity.{Percentage, Quantity}
import starling.models.{European, Call}

class BarrelPositionTests extends TestMarketSpec {

	private val prices:Map[CommodityMarket, Double] = Map(
    LME_LEAD -> 123.0, NYMEX_WTI -> 108.0,
    NYMEX_SINGAPORE_FUEL_OIL -> 77.0,
    NYMEX_GASOLINE -> 88.0)
	def env(zeroRate : Double) = Environment(
			new TestingAtomicEnvironment(){
				def applyOrMatchError(key : AtomicDatumKey) = key match {
					case DiscountRateKey(ccy, day, _) => new Quantity(math.exp(- zeroRate * day.daysSinceInYears(marketDay.day)))
					case ForwardPriceKey(market, _, _) => Quantity(prices(market), market.priceUOM)
          case USDFXRateKey(ccy) => Quantity(1.0, USD/ccy)
          case _ : OilAtmVolAtomicDatumKey => new Percentage(0)
          case _ : OilVolSkewAtomicDatumKey => Map[Double, Percentage]()
				}
				def marketDay : DayAndTime = Day(2009, 10, 1).endOfDay

			}) 
			
	@Test
	def testWTIFutureBarrelPositionMatchesVolume{
    val volume: Quantity = Quantity(1000, BBL)
    val future = Future(
				NYMEX_WTI, Month(2010, 10),
				Quantity(1234.0, USD/BBL), volume
		)
		val position = future.oilBarrelPosition(env(0.05))
    assertQtyEquals(position, volume, 1e-6)
	}

  @Test
  def testLeadFutureHasNullBarrelPosition{
    val future = Future(
				LME_LEAD, Day(2010, 10, 10),
				Quantity(1234.0, USD/MT), Quantity(100, MT)
		)
		val position = future.oilBarrelPosition(env(0.05))
    assertQtyEquals(position, Quantity.NULL, 1e-6)
  }

  @Test
  def testGasolineDeepITMOptionPosition{
    val gallonVolume: Quantity = Quantity(1000, GAL)
    val option = new FuturesOption(
      NYMEX_GASOLINE, Day(2010, 10, 10), Month(2010, 11), Quantity(0.01, USD/GAL),
      gallonVolume, Call, European)
    val bblPosition = option.oilBarrelPosition(env(0.0))
    val expectedBblPosition = NYMEX_GASOLINE.convertUOM(gallonVolume, BBL)
    assertQtyEquals(bblPosition, expectedBblPosition, 1e-6)
  }

  @Test
  def testCrossCurrencyPosition{
    val volume: Quantity = Quantity(1000, BBL)
    val future = Future(
				NYMEX_WTI, Month(2010, 10),
				Quantity(1234.0, GBP/BBL), volume
		)
		val position = future.oilBarrelPosition(env(0.05))
    assertQtyEquals(position, volume, 1e-6)
  }

  @Test
  def testCrossCurrencyCrossUOMPosition{
    val volume: Quantity = Quantity(1000, MT)
    val future = Future(
				NYMEX_WTI, Month(2010, 10),
				Quantity(1234.0, GBP/MT), volume
		)
		val position = future.oilBarrelPosition(env(0.05))
    val volumeBBL = NYMEX_WTI.convert(volume, BBL).get
    assertQtyEquals(position, volumeBBL, 1e-6)
  }

  @Test
  def testFuelOilConvertsFromTonnes{
    val volume: Quantity = Quantity(1000, MT)
    val future = Future(
        NYMEX_SINGAPORE_FUEL_OIL, Month(2010, 10),
        Quantity(1234.0, USD/MT), volume
    )
    val position = future.oilBarrelPosition(env(0.05))
    assertQtyEquals(position, NYMEX_SINGAPORE_FUEL_OIL.convert(volume, BBL).get, 1e-6)
  }
}
