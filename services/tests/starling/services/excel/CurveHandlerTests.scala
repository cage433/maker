package starling.services.excel

import org.testng.annotations.Test
import starling.utils.StarlingTest
import org.mockito.Mockito._
import starling.db.MarketDataStore
import starling.daterange.Day
import starling.pivot.controller.PivotTable
import org.scalatest.matchers.ShouldMatchers
import starling.gui.api._
import starling.curves.{EnvironmentSpecification, ClosesEnvironmentRule, CurveViewer}
import starling.pivot.{PivotQuantity, Field, PivotFieldsState}
import starling.quantity.UOM


class CurveHandlerTests extends StarlingTest with ShouldMatchers {
  @Test
  def shouldReadSingleValueFromCurve {
    val curveViewer = mock(classOf[CurveViewer])
    val marketDataStore = mock(classOf[MarketDataStore])
    val pivotTable = mock(classOf[PivotTable])

    val curveHandler = new CurveHandler(curveViewer, marketDataStore)

    val pricingGroup: PricingGroup = PricingGroup.Metals
    val selection: MarketDataSelection = MarketDataSelection(Some(pricingGroup))
    val observationDay: Day = Day.today
    val environmentRule = ClosesEnvironmentRule.label
    val environmentSpecification = EnvironmentSpecificationLabel(observationDay, environmentRule)
    val dataIdentifier: MarketDataIdentifier = MarketDataIdentifier(selection, SpecificMarketDataVersion(123))
    val fieldsState: PivotFieldsState = PivotFieldsState(dataFields = List(Field("Price")),
      rowFields = List(Field("Month"), Field("Market")))

    when(marketDataStore.latestMarketDataIdentifier(selection)) thenReturn dataIdentifier
    when(curveViewer.pricePivotTableFor(dataIdentifier, environmentSpecification, fieldsState)) thenReturn pivotTable
    when(pivotTable.cell(Field("Price"), Field("Month") → "<month>", Field("Market") → "<market>").asInstanceOf[PivotQuantity]).
      thenReturn(new PivotQuantity(123.456, UOM.BUSHEL_WHEAT))

    val value = curveHandler.curveValue("Price", Map(
      "Pricing Group" → pricingGroup.name,
      "Day" → observationDay.toExcel,
      "Rule" → environmentRule.name), "Month", "<month>", "Market", "<market>")

    value should be === 123.456
  }
}
