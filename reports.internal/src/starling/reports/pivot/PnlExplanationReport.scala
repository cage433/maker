package starling.reports.pivot

import starling.tradestore.TradeSet
import starling.daterange.{Day, Timestamp}
import starling.quantity.UOM
import starling.pivot.PivotFieldsState._
import starling.pivot.Field._
import starling.utils.Log
import starling.pivot.{Field, PivotFieldsState, UnionPivotTableDataSource, NullPivotTableDataSource}
import starling.curves.{AtomicEnvironment, Environment, ForwardStateEnvironment}

object PnlExplanationReport {
  def run(d1: AtomicEnvironment,
          d2: AtomicEnvironment,
          tradeSet: TradeSet,
          curveIdentifierDm1: CurveIdentifier,
          curveIdentifierD: CurveIdentifier,
          tM1: Timestamp,
          t: Timestamp,
          expiryDay: Day, addRows: Boolean) = {

    val marketAndTimePivot = if (d1.marketDay == d2.marketDay) {
      NullPivotTableDataSource
    } else {
      val utps = tradeSet.utps(curveIdentifierD.tradesUpToDay, expiryDay, t)
      val d1Fwd = new ForwardStateEnvironment(d1, d2.marketDay)
      val marketChanges = new MarketChangesPnl(d1Fwd, d2, utps)
      val timeChanges = new TimeChangesPnl(Environment(d1), d2.marketDay, utps)
      val priceChange = new CurrentPriceChangeReport(Environment(d1), Environment(d2), utps)
      val list = List(
        PivotReportData.run(marketChanges, utps, SlideDetails.Null, List()),
        PivotReportData.run(priceChange, utps, SlideDetails.Null, List()),
        PivotReportData.run(timeChanges, utps, SlideDetails.Null, List())
      )
      val tradePivot = tradeSet.reportPivot(curveIdentifierDm1.tradesUpToDay, expiryDay, t, addRows)
      new ReportPivotTableDataSource(tradePivot, list)
    }
    val newTradesPivot = {
      val newTradesTradeSet = tradeSet.forTradeDays((d1.marketDay.day upto d2.marketDay.day).toSet - d1.marketDay.day)
      val utps = newTradesTradeSet.utps(curveIdentifierD.tradesUpToDay, expiryDay, t)
      val newTradesReport = new NewTradesPivotReport(Environment(d2), UOM.USD, utps)
      val list = List(PivotReportData.run(newTradesReport, utps, SlideDetails.Null, List()))
      val tradePivot = newTradesTradeSet.reportPivot(curveIdentifierD.tradesUpToDay, expiryDay, t, addRows)
      new ReportPivotTableDataSource(tradePivot, list) {
        val dayChangeText = "Day Change"

        override def initialState = PivotFieldsState(
          dataFields = List(Field(dayChangeText)),
          rowFields = List(Field("Risk Period")),
          columnFields = List(Field("Risk Market"), Field("Day Change Component"))
        )

        override def zeroFields = Set(Field(dayChangeText))
      }
    }
    val tradeChangesPivot = {
      if (tM1 == t) {
        None
      } else {
        val tradeChanges = Log.infoWithTime("Trade Changes read") {
          tradeSet.tradeChanges(tM1, t, expiryDay)
        }
        val rows = new TradeChangesPnl(Environment(d1)).rows(tradeChanges.removeUntraded(d1.marketDay.day))
        Some(new TradeChangesPnlPivotTableDataSource(tradeChanges.fields, d2.marketDay, rows))
      }
    }
    val pivot = UnionPivotTableDataSource.join(List(newTradesPivot, marketAndTimePivot) ::: tradeChangesPivot.toList)
    pivot
  }
}