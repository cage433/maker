package starling.reports.impl.pivot

import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.pivot.FieldDetailsGroup._
import starling.tradestore.TradeSet
import starling.pivot._
import starling.gui.api.{Desk, ReportParameters}
import starling.utils.cache.CacheFactory
import starling.utils.{StringIO, AppendingMap}
import starling.daterange.{Day, Timestamp}

/**
 * Report producing: YTD = (MTM - Aspect MTM at year end).
 */
class YearToDateReport {

  private val cache = CacheFactory.getCache("YearToDateReportAspectValuesCache")

  private val deskFileMap = Map(Desk.GasoilSpec -> "/starling/reports/impl/GasOilSpec_Yearly_1011.txt.gz")

  private def aspectValues(desk: Desk) = cache.memoize((desk), {
    val file = deskFileMap.get(desk)
    file.map(f => StringIO.linesFromZipped(f).map {
      case line => line.split('\t').toList match {
        case (tradeID :: act :: mtm :: Nil) => (tradeID -> (Quantity(mtm.toDouble, USD) - Quantity(act.toDouble, USD)))
      }
    }.toMap)
  })

  def report(reportContextBuilder: ReportContextBuilder, curveIdentifierFactory: CurveIdentifierFactory, tradeSets: List[(TradeSet, Timestamp)], reportParameters: ReportParameters): List[UnfilteredPivotTableDataSource {def unfilteredData(pfs: PivotFieldsState): List[AppendingMap[Field, Any]]; def fieldDetailsGroups: List[FieldDetailsGroup]}] = {
    val desks = tradeSets.flatMap {
      case (ts, _) => {
        ts.tradePredicate.filter.flatMap {
          case (field, SomeSelection(values)) if field == new Field("Desk") => Some(values)
          case _ => None
        }
      }
    }.flatten.distinct.map {
      case d => Desk.fromName(d.toString)
    }
    val startOfFinancialYear = Day(2011, 10, 1) // we only do 2011 at the moment

    if (desks.forall(deskFileMap.contains)) {
      val endOfYear = (Map[String, Quantity]() /: desks.map(aspectValues(_).get))(_ ++ _)

      val env = reportContextBuilder.contextFromCurveIdentifier(curveIdentifierFactory.unLabel(reportParameters.curveIdentifier)).environment
      val yearToDateField = new SumPivotQuantityFieldDetails("Ytd")
      //      val marketDay = reportParameters.curveIdentifier.valuationDayAndTime
      tradeSets.map {
        case (tradeSet, ts) => {
          val (fieldGroups, trades) = tradeSet.readAll(ts)
          val rows = trades.map {
            tradeAndFields => {
              val trade = tradeAndFields.trade
              val costs = if(trade.tradeDay < startOfFinancialYear) {
                PivotQuantity.sum(trade.costs.map(c => PivotQuantity.calcOrCatch(c.mtm(env, USD))))
              } else {
                PivotQuantity.NULL
              }

              val mtm = PivotQuantity.calcOrCatch(trade.mtm(env))
              val aspectMtm = endOfYear.getOrElse(trade.tradeID.toString, Quantity.NULL)
              val yearToDate = mtm - aspectMtm.pq - costs
              tradeAndFields.fields.add("ytd", Map(yearToDateField.field -> yearToDate))
            }
          }
          new UnfilteredPivotTableDataSource() {
            def unfilteredData(pfs: PivotFieldsState) = rows

            def fieldDetailsGroups = FieldDetailsGroup("Report Fields", yearToDateField :: Nil) :: fieldGroups
          }
        }
      }
    } else {
      Nil
    }
  }
}