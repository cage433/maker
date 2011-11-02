package starling.services

import starling.daterange.Day
import starling.market.FuturesExchange
import starling.marketdata.PriceDataType
import starling.pivot._
import controller.CombinedCell

import starling.utils.ImplicitConversions._
import scalaz.Scalaz._
import starling.rmi.FC2Service
import starling.scheduler.EmailingScheduledTask
import starling.gui.api.{Email, MarketDataSelection}

class VerifyPricesValid(dataSource: PivotTableDataSource, exchange: FuturesExchange, emailService: EmailService,
                        template: Email) extends EmailingScheduledTask(emailService, template) {

  private val pfs = PivotFieldsState(rowFields = fields("Market", "Period"), dataFields = fields("Price"))

  def emailFor(observationDay: Day): Option[Email] = {
    val filter = filters("Exchange" → exchange.name, "Observation Day" → observationDay)
    val grid = dataSource.gridFor(Some(pfs.copy(filters = filter ++ filters("Validity" → "Invalid"))))

    (grid.hasData).option {
      template.copy(subject = "Validation errors for: %s" % filterToString(filter),
        body = <span>
                 <p>Validation errors for: { filterToString(filter) }</p>
                 <table border="1">
                   {for (row <- grid.combinedData) yield
                     <tr>{ row.map(createCell) }</tr>
                   }
                 </table>
               </span>.toString)
    }
  }

  private def createCell(cell: CombinedCell) = <td bgcolor={cellColour(cell)}>{cell}</td>
  private def cellColour(cell: CombinedCell): String = cell match {
    case CombinedCell.TableCell(TableCell.PivotQuantity(pq)) => pq.errorState.fold("white", "orange", "red")
    case CombinedCell.AxisCell(_) => "white"
  }
}
