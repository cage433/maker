package starling.services

import starling.daterange.Day
import starling.db.MarketDataStore
import starling.gui.api.{EmailEvent, MarketDataSelection, PricingGroup}
import starling.market.FuturesExchange
import starling.marketdata.PriceDataType
import starling.pivot._
import starling.utils.Broadcaster
import starling.utils.ImplicitConversions._


object VerifyPricesValid {
  def apply(marketDataStore: MarketDataStore, pricingGroup: PricingGroup, exchange: FuturesExchange, broadcaster: Broadcaster,
            from: String, to: String*) = new VerifyPricesValid(
    marketDataStore.pivot(MarketDataSelection(Some(pricingGroup)), PriceDataType), exchange, broadcaster, from, to : _*)
}

class VerifyPricesValid(dataSource: PivotTableDataSource, exchange: FuturesExchange,  broadcaster: Broadcaster, from: String, to: String*)
  extends BroadcastingScheduledTask(broadcaster) {

  private val pfs = PivotFieldsState(rowFields = fields("Market", "Period"), dataFields = fields("Price"))

  def eventFor(observationDay: Day): Option[EmailEvent] = {
    val filter = filters("Exchange" → exchange.name, "Observation Day" → observationDay)
    val grid = dataSource.gridFor(Some(pfs.copy(filters = filter ++ filters("Validity" → "Invalid"))))

    (grid.hasData).toOption {
      EmailEvent(from, to).copy(subject = "Validation errors for: %s" % filterToString(filter),
        body = <html>
                 <p>Validation errors for: { filterToString(filter) }</p>
                 <table border="1">
                   {for (row <- grid.combinedData) yield
                     <tr>{ row.map(createCell) }</tr>
                   }
                 </table>
               </html>.toString)
    }
  }

  private def createCell(cell: Any) = <td bgcolor={pivotQuantityCellColour(cell).getOrElse("white")}>{cell}</td>
  private def pivotQuantityCellColour(cell: Any): Option[String] = cell partialMatch {
    case PivotQuantityCell(pq) => if (pq.hasErrors) "red" else if (pq.hasWarning) "orange" else "white"
  }

  object PivotQuantityCell {
    def unapply(cell: Any): Option[PivotQuantity] = cell partialMatch {
      case tc: TableCell if tc.value.isInstanceOf[PivotQuantity] => tc.value.asInstanceOf[PivotQuantity]
    }
  }
}
