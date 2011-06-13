package starling.services.rpc.marketdata

import com.trafigura.edm.marketdata._
import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions.asJavaList
import starling.db.MarketDataStore
import starling.gui.api.{MarketDataSelection, PricingGroup, MarketDataIdentifier}
import starling.marketdata.MarketDataTypes
import starling.pivot.model.PivotTableModel
import starling.utils.ImplicitConversions._
import starling.services.Server
import starling.pivot._
import starling.utils.Log
import com.trafigura.edm.physicaltradespecs.EDMQuota


/**
 * Implementation of the market data service stub, provides market data using supplied parameters,
 *   this service operation is generic and covers all market data
 */
class MarketDataServiceRPC(marketDataStore: MarketDataStore) extends MarketDataService {

  // TODO [10 Jun 2011] Ensure missing or invalid data causes nice exceptions to be thrown
  def marketData(parameters: MarketDataRequestParameters) : MarketDataResponse = {

    Log.info("%s called with parameters %s".format(this.getClass.getName, parameters))

    val selection = MarketDataSelection(Some(PricingGroup.fromName(parameters.pricingGroup)))
    val version = parameters.version.getOrElse(marketDataStore.latest(selection))

    val pivot = marketDataStore.pivot(MarketDataIdentifier(selection, version), MarketDataTypes.fromName(parameters.dataType))

    val filters = parameters.filters.map(filter =>
      (Field(filter.name), SomeSelection(filter.value.map(v => pivot.lookup(filter.name).parser.parse(v)._1).toSet)))

    val pfs = PivotFieldsState(fields(parameters.measures), fields(parameters.rows), fields(parameters.columns), filters)

    val data = PivotTableModel.createPivotData(pivot, PivotFieldParams(true, Some(pfs))).pivotTable.toFlatRows(Totals.Null)

    MarketDataResponse(parameters.update(_.version = Some(version)), data.map(row => MarketDataColumnData(row.map(_.toString))))
  }

  /**
   * get price fixings for the supplied EDM Quota
   */
  def getFixings(parameters: EDMQuota) : MarketDataResponse = {
    new MarketDataResponse()
  }

  private def fields(names: List[String]) = names.map(Field(_))
}

/**
 * Market data service stub
 *  this service stub impl that overrides the filter chain with a null implementation
 */
class MarketDataServiceResourceStubEx()
    extends MarketDataServiceResourceStub(new MarketDataServiceRPC(Server.server.marketDataStore), List[ServiceFilter]()) {

  // this is deliberately stubbed out as the exact requirements on permissions and it's implementation for this service is TBD
  override def requireFilters(filterClasses:String*) {}
}
