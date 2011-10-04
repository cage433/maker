package starling.curves

import starling.utils.StarlingTest
import org.scalatest.matchers.ShouldMatchers
import starling.market.Market
import org.mockito.Mockito._
import starling.daterange._
import starling.marketdata._
import ObservationTimeOfDay._
import org.testng.annotations.{BeforeMethod, Test}
import starling.db.MarketDataReader
import starling.gui.api.{EnvironmentRuleLabel, MarketDataSelection, MarketDataIdentifier}


class VanillaEnvironmentRuleTests extends StarlingTest with ShouldMatchers {
  def rule = new VanillaEnvironmentRule(observationPoint.copyDay(_), TimeOfDay.EndOfDay, EnvironmentRuleLabel.COB, Nil, ReferenceDataLookup.Null)

  var reader: MarketDataReader = _

  val marketDataIdentifier = MarketDataIdentifier(MarketDataSelection(), 0)
  val observationPoint = ObservationPoint(Day.today, LMEClose)

  @BeforeMethod
  def init() {
    reader = mock(classOf[MarketDataReader])
  }

  @Test
  def hasNoMarketsWhenThereIsNoMarketData {
    when(reader.readAllPrices(observationPoint)) thenReturn Nil
    when(reader.readAllVols(observationPoint)) thenReturn Nil
    rule.createEnv(observationPoint.day.get, reader).markets should be === Nil
  }

  @Test
  def shouldHaveMarketsBasedOnAvailablePriceData {
    val priceData = PriceDataKey(Market.LME_LEAD) →
      PriceData.create(Map(Day.today.containingMonth → 456.789), Market.LME_LEAD.priceUOM)

    when(reader.readAllPrices(observationPoint)).thenReturn(priceData :: Nil)
    when(reader.readAllVols(observationPoint)) thenReturn Nil

    val expectedMarkets = UnderlyingDeliveryPeriods(observationPoint.timeOfDay, Market.LME_LEAD, priceData._2.sortedKeys) :: Nil
    rule.createEnv(observationPoint.day.get, reader).markets should be === expectedMarkets
  }
}