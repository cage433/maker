package starling.db

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import starling.daterange.{ObservationTimeOfDay, Day, ObservationPoint}
import starling.utils.StarlingSpec
import starling.pivot.{PivotQuantity, Row}
import starling.quantity.{Percentage, UOM, Quantity}
import starling.market.{TestMarketSpec, Market}
import starling.marketdata._

class MarketDataIDTests extends WordSpec with TestMarketSpec with ShouldMatchers {
  val priceMarketDataID = new MarketDataID(ObservationPoint(Day.today, ObservationTimeOfDay.LMEClose), MarketDataSet.LimMetals,
    PriceDataKey(Market.LME_COPPER), new MarketDataTypes(ReferenceDataLookup.Null))
  val freightMarketDataID = priceMarketDataID.copy(subTypeKey =
    FreightParityDataKey(IncotermCode("IC"), ContractualLocationCode("CLC"), IncotermCode("IC"), NeptuneCountryCode("NCC")))

  val noComment: String = null

  "should be able to extract Quantities" in {
    extractValueFromPrice(Quantity(1.234, UOM.USD / UOM.MT)) should be === Some(((UOM.USD / UOM.MT).toString, 1.234, noComment))
  }

  "should be able to extract Percentages" in {
    extractValueFromPrice(Percentage(1.234)) should be === Some(("%", 123.4, noComment))
  }

  "should be able to extract PivotQuantities" in {
    extractValueFromPrice(Quantity(1.234, UOM.USD / UOM.MT).pq) should be === Some(((UOM.USD / UOM.MT).toString, 1.234, noComment))
  }

  "should be able to extract comments" in {
    extractValueFromFreight(Quantity(1.234, UOM.USD / UOM.MT), "a comment") should be ===
      Some(((UOM.USD / UOM.MT).toString, 1.234, "a comment"))
  }

  private def extractValueFromPrice(value: Any) = priceMarketDataID.extractValue(Row(PriceDataType.priceField.field → value))
  private def extractValueFromFreight(value: Any, comment: String) = freightMarketDataID.extractValue(Row(
    new FreightParityDataType().parityRateField.field → value, new FreightParityDataType().commentField.field → comment))
}