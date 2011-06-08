package starling.services.rpc.marketdata

import com.trafigura.edm.marketdata._
import com.trafigura.tradinghub.support._

import starling.db.MarketDataStore
import starling.gui.api.{MarketDataSelection, PricingGroup, MarketDataIdentifier}
import starling.marketdata.MarketDataTypes
import starling.pivot.model.PivotTableModel
import starling.pivot.{Totals, Field, PivotFieldsState, PivotFieldParams}

import starling.utils.ImplicitConversions._


class MarketDataServiceRPC(marketDataStore: MarketDataStore) extends MarketDataService {
  def marketData(parameters: MarketDataRequestParameters): MarketDataResponse = {
    val selection = MarketDataSelection(Some(PricingGroup.fromName(parameters.pricingGroup)))
    val version = parameters.version.getOrElse(marketDataStore.latest(selection))

    val pivot = marketDataStore.pivot(MarketDataIdentifier(selection, version), MarketDataTypes.fromName(parameters.dataType))
    val pfs = PivotFieldsState(fields(parameters.measures), fields(parameters.rows), fields(parameters.columns))

    val data = PivotTableModel.createPivotData(pivot, PivotFieldParams(true, Some(pfs))).pivotTable.toFlatRows(Totals.Null)

    MarketDataResponse(parameters.update(_.version = Some(version)), data.map(row => MarketDataColumnData(row.map(_.toString))))
  }

  private def fields(names: List[String]) = names.map(Field(_))
  //override def requireFilters(filterClasses:String*) {}
}

/**
 * temporary service stub impl that overrides the filter chain with a null implementation
 */
class MarketDataServiceResourceStubEx(target: MarketDataService, filters: java.util.List[ServiceFilter])
    extends MarketDataServiceResourceStub(target, filters) {

  override def requireFilters(filterClasses:String*) {}
}
