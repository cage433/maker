package starling.services

import starling.daterange.Day
import starling.db.MarketDataStore
import starling.gui.api.{EmailEvent, MarketDataSelection}
import starling.market.FuturesExchange
import starling.marketdata.PriceDataType
import starling.pivot._
import controller.CombinedCell
import starling.utils.Broadcaster

import starling.utils.ImplicitConversions._


object VerifyPricesValid {
  def apply(marketDataStore: MarketDataStore, broadcaster: Broadcaster, dataFlow: DataFlow) = new VerifyPricesValid(
    marketDataStore.pivot(MarketDataSelection(Some(dataFlow.pricingGroup)), PriceDataType), dataFlow.exchange, broadcaster, dataFlow.from, dataFlow.to)
}

class VerifyPricesValid(dataSource: PivotTableDataSource, exchange: FuturesExchange, broadcaster: Broadcaster,
                        from: String, to: String) extends EmailingScheduledTask(broadcaster, from, to) {

  private val pfs = PivotFieldsState(rowFields = fields("Market", "Period"), dataFields = fields("Price"))

  def eventFor(observationDay: Day, email: EmailEvent): Option[EmailEvent] = {
    val filter = filters("Exchange" → exchange.name, "Observation Day" → observationDay)
    val grid = dataSource.gridFor(Some(pfs.copy(filters = filter ++ filters("Validity" → "Invalid"))))

    (grid.hasData).toOption {
      email.copy(subject = "Validation errors for: %s" % filterToString(filter),
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

  private def createCell(cell: CombinedCell) = <td bgcolor={cellColour(cell)}>{cell}</td>
  private def cellColour(cell: CombinedCell): String = cell match {
    case CombinedCell.TableCell(TableCell.PivotQuantity(pq)) => pq.errorState.fold("white", "orange", "red")
    case CombinedCell.AxisCell(_) => "white"
  }
}
