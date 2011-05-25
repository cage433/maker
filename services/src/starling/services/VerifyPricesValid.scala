package starling.services

import collection.immutable.List._
import starling.daterange.Day
import starling.db.MarketDataStore
import starling.marketdata.PriceDataType
import starling.pivot._
import starling.props.{Props, PropsHelper}
import starling.utils.ImplicitConversions._
import starling.market.FuturesExchange
import starling.gui.api.{EmailEvent, MarketDataSelection, PricingGroup}
import starling.utils.Broadcaster

object VerifyPricesValid {
  def apply(marketDataStore: MarketDataStore, pricingGroup: PricingGroup, exchange: FuturesExchange, broadcaster: Broadcaster,
            from: Props => PropsHelper#Property, to: (Props => PropsHelper#Property)*) = new VerifyPricesValid(
    marketDataStore.pivot(MarketDataSelection(Some(pricingGroup)), PriceDataType), exchange, broadcaster, from, to : _*)
}


class VerifyPricesValid(dataSource: PivotTableDataSource, exchange: FuturesExchange,  broadcaster: Broadcaster,
                        from: Props => PropsHelper#Property, to: (Props => PropsHelper#Property)*)
  extends BroadcastingScheduledTask(broadcaster) {

  private val email = EmailEvent(from = from(PropsHelper.defaultProps).value)
  private val pfs = PivotFieldsState(rowFields = fields("Market", "Period"), dataFields = fields("Price"))

  def eventFor(observationDay: Day): Option[EmailEvent] = {
    val filter: scala.List[(Field, Selection)] = filters("Exchange" → exchange.name, "Observation Day" → observationDay)
    val grid = dataSource.gridFor(Some(pfs.copy(filters = filter ++ filters("Validity" → "Invalid"))))

    (grid.hasData).toOption {
      val body = <html>
                  <p>Validation errors for: { filterToString(filter) }</p>

                  <table border="1">
                    {for (row <- grid.combinedData) yield
                      <tr>{ row.map(createCell) }</tr>
                    }
                  </table>
                </html>

      email.copy(to = to.map(f => f(PropsHelper.defaultProps).value),
                 subject = "Validation errors for: %s" % filterToString(filter),
                 body = body.toString)
    }
  }

  private def createCell(cell: Any) = <td bgcolor={background(cell)}>{cell}</td>
  private def background(cell: Any) = cell match {
    case PivotQuantityCell(pq) => if (pq.hasErrors) "red" else if (pq.hasWarning) "orange" else "white"
    case _ => "white"
  }

  object PivotQuantityCell {
    def unapply(cell: Any): Option[PivotQuantity] = cell partialMatch {
      case tc: TableCell if tc.value.isInstanceOf[PivotQuantity] => tc.value.asInstanceOf[PivotQuantity]
    }
  }
}
