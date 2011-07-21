package starling.services.excel

import starling.db.MarketDataStore
import starling.daterange.Day
import starling.loopyxl.ExcelMethod
import starling.curves.CurveViewer
import starling.pivot.{PivotQuantity, PivotFieldsState, Field}
import starling.gui.api.{EnvironmentRuleLabel, EnvironmentSpecificationLabel, MarketDataSelection, PricingGroup}

class CurveHandler(curveViewer: CurveViewer, marketDataStore: MarketDataStore) {
  @ExcelMethod
  def curveValue(measure: String, dataSourceParameters: Map[String, Any], filters : AnyRef*): Any = {
    val pricingGroup = PricingGroup.fromName(dataSourceParameters("Pricing Group").asInstanceOf[String])
    val marketDataIdentifier = marketDataStore.latestMarketDataIdentifier(MarketDataSelection(Some(pricingGroup)))
    val observationDay = Day.fromExcel(dataSourceParameters("Day").asInstanceOf[Double])
    val envSpec = EnvironmentSpecificationLabel(observationDay,
      EnvironmentRuleLabel(dataSourceParameters("Rule").asInstanceOf[String]))

    val measureField: Field = Field(measure)
      val filtersAsField = filters.grouped(2).map(_.toList).map(x => (x : @unchecked) match { case List(field:String, value) => (Field(field), value)}).toMap

    val pivotFieldsState = PivotFieldsState(dataFields = List(measureField), rowFields = filtersAsField.keys.toList)
    val pivotTable = curveViewer.pricePivotTableFor(marketDataIdentifier, envSpec, pivotFieldsState)

    pivotTable.cell(measureField, filtersAsField.toSeq: _ *).asInstanceOf[PivotQuantity].doubleValue.getOrElse("")
  }
}
