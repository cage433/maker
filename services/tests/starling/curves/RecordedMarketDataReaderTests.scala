package starling.curves

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import starling.daterange._, Day._
import starling.marketdata._
import starling.utils.StarlingSpec
import starling.market.Market
import starling.quantity._


class RecordedMarketDataReaderTests extends StarlingSpec with ShouldMatchers {
  "readAllPrices only returns (PriceDataKey, PriceData)" in {
    reader.readAllPrices(observationPoint) should be === List((priceDataKey, priceData))
  }

  "readAllVols only returns (OilVolSurfaceDataKey, OilVolSurfaceData)" in {
    reader.readAllVols(observationPoint) should be === List((oilKey, oilData))
  }

  "readAll only returns matching subset" in {
    reader.readAll(PriceDataType, observationPoint) should be === List((priceDataKey, priceData))
    reader.readAll(OilVolSurfaceDataType, observationPoint) should be === List((oilKey, oilData))
  }

  private lazy val observationPoint = ObservationPoint(16 Jan 2012, ObservationTimeOfDay.Default)
  private lazy val priceDataKey = PriceDataKey(Market.COMEX_GOLD)
  private lazy val priceData = PriceData(Map((16 Jan 2012) → Quantity(1.234, UOM.GBP).pq))
  private lazy val oilKey = OilVolSurfaceDataKey(Market.NYMEX_WTI)
  private lazy val oilData = OilVolSurfaceData(Array(16 Jan 2012), Array(Percentage(4)), Array.empty, Array.empty)
  private lazy val data = List(
    (TimedMarketDataKey(observationPoint, priceDataKey), priceData), (TimedMarketDataKey(observationPoint, oilKey), oilData)
  )

  private lazy val reader = new RecordedMarketDataReader("identifier", data, new MarketDataTypes(ReferenceDataLookup.Null))
}