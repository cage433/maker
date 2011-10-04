package starling.curves

import starling.utils.StarlingTest
import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.Test
import starling.pivot.{Field, PivotFieldsState, PivotQuantity}
import starling.quantity.{UOM, Quantity}
import scala.collection.immutable.{TreeSet, TreeMap}
import starling.db.MarketDataReader
import org.mockito.Mockito._
import starling.daterange._
import starling.marketdata._
import starling.market.{TestMarketTest, FuturesMarket, FuturesExchangeFactory, Market}

class TimeShiftingEnvironmentRuleTests extends TestMarketTest with ShouldMatchers {
  val timeShiftingRule = new TimeShiftToLMECloseEnvironmentRule(ReferenceDataLookup.Null)

  val shanghaiPriceAtShanghaiClose = Quantity(16440, Market.SHANGHAI_COPPER.priceUOM)
  val lmePriceAtShanghaiClose = Quantity(2524, Market.LME_COPPER.priceUOM)
  val lmePriceAtLmeClose = Quantity(2563.75, Market.LME_COPPER.priceUOM)
  val fxAtShanghaiClose = Quantity(1 / 6.55, UOM.USD / UOM.CNY)
  val fxAtLmeClose = Quantity(1 / 6.55, UOM.USD / UOM.CNY)

  val timeShiftedShanghaiPrice = {
    val shanghaiPriceInUSD = shanghaiPriceAtShanghaiClose * fxAtShanghaiClose
    val shift = shanghaiPriceInUSD - lmePriceAtShanghaiClose
    (lmePriceAtLmeClose + shift) / fxAtLmeClose
  }


  def priceEntry(day:Day, timeOfDay:ObservationTimeOfDay, market:FuturesMarket, price:Quantity) = {
    val prices = PriceData.fromMap(Map(market.frontPeriod(day) → price))
    TimedMarketDataKey(ObservationPoint(day, timeOfDay), PriceDataKey(market)) → prices
  }
  def fxEntry(day:Day, timeOfDay:ObservationTimeOfDay, rate:Quantity) = {
    TimedMarketDataKey(ObservationPoint(day, timeOfDay), SpotFXDataKey(rate.uom.denominatorUOM)) → SpotFXData(rate)
  }
  import ObservationTimeOfDay._

  @Test
  def shangaiPriceAtLmeCloseShouldBeSameDayShanghaiClosePriceTimeShiftedWhenShanghaiIsOpen {
    val environmentDay = Day(2011, 1, 5)
    val reader = new RecordedMarketDataReader(
      "<TimeShiftingEnvironmentRuleTests>",
      List(
        priceEntry(environmentDay, SHFEClose, Market.SHANGHAI_COPPER, shanghaiPriceAtShanghaiClose),
        priceEntry(environmentDay, SHFEClose, Market.LME_COPPER, lmePriceAtShanghaiClose),
        priceEntry(environmentDay, LMEClose, Market.LME_COPPER, lmePriceAtLmeClose),
        fxEntry(environmentDay, LondonClose, fxAtLmeClose),
        fxEntry(environmentDay, SHFEClose, fxAtShanghaiClose)
      )
    )

    val frontPeriod = Market.LME_COPPER.frontPeriod(environmentDay)

    val shiftedEnvironment = timeShiftingRule.createEnv(environmentDay, reader).environment

    shiftedEnvironment.forwardPrice(Market.LME_COPPER, frontPeriod) should be === lmePriceAtLmeClose
    timeShiftedShanghaiPrice.uom should be === Market.SHANGHAI_COPPER.priceUOM
    val actualPrice = shiftedEnvironment.forwardPrice(Market.SHANGHAI_COPPER, Market.SHANGHAI_COPPER.frontPeriod(environmentDay))
    actualPrice should be  === timeShiftedShanghaiPrice
  }

  @Test
  def shangaiPriceAtLmeCloseShouldBePreviousDayShanghaiClosePriceWhenShanghaiIsClosed {

    val environmentDay = Day(2011, 4, 5) //Shanghai is closed Mon 4th Apr And Tue 5th Apr see TestHolidays
    val lastShanghaiDay = Day(2011, 4, 1)
    val reader = new RecordedMarketDataReader(
      "<TimeShiftingEnvironmentRuleTests>",
      List(
        priceEntry(lastShanghaiDay, SHFEClose, Market.SHANGHAI_COPPER, shanghaiPriceAtShanghaiClose),
        priceEntry(lastShanghaiDay, SHFEClose, Market.LME_COPPER, lmePriceAtShanghaiClose),
        priceEntry(environmentDay, LMEClose, Market.LME_COPPER, lmePriceAtLmeClose),
        fxEntry(environmentDay, LondonClose, fxAtLmeClose),
        fxEntry(lastShanghaiDay, SHFEClose, fxAtShanghaiClose)
      )
    )

    val shiftedEnvironment = timeShiftingRule.createEnv(environmentDay, reader).environment

    shiftedEnvironment.forwardPrice(Market.LME_COPPER, Market.LME_COPPER.frontPeriod(environmentDay)) should be === lmePriceAtLmeClose

    timeShiftedShanghaiPrice.uom should be === Market.SHANGHAI_COPPER.priceUOM

    val actualPrice = shiftedEnvironment.forwardPrice(Market.SHANGHAI_COPPER, Market.SHANGHAI_COPPER.frontPeriod(environmentDay))
    actualPrice should be  === timeShiftedShanghaiPrice
  }
}
