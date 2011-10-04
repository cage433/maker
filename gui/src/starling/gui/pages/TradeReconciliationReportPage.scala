package starling.gui.pages

import starling.daterange.Timestamp
import starling.gui.StarlingServerContext
import starling.gui.api.{IntradayUpdated, TradeTimestamp, TradeSelection}
import collection.mutable.ListBuffer
import swing.event.Event
import starling.pivot.PivotEdits
import starling.browser.{LocalCache, Page}
import starling.gui.StarlingLocalCache._

case class TradeReconciliationReportPage(tradeSelection:TradeSelection, from:TradeTimestamp, to:TradeTimestamp,
                                         intradayTimestamp: Timestamp, pivotPageState:PivotPageState) extends AbstractStarlingPivotPage(pivotPageState) {
  def text = "Trade Reconciliation"

  def selfPage(pivotPageState: PivotPageState, edits:PivotEdits) = copy(pivotPageState = pivotPageState)

  def dataRequest(pageBuildingContext:StarlingServerContext) = {
    pageBuildingContext.cachingStarlingServer.tradeReconciliation(tradeSelection, from, to, intradayTimestamp, pivotPageState.pivotFieldParams)
  }

  override def latestPage(localCache:LocalCache) = {
    tradeSelection.intradaySubgroup match {
      case Some(groups) => {
        val latestTimestamp = localCache.latestTimestamp(groups)
        copy(intradayTimestamp = latestTimestamp)
      }
      case _ => this
    }
  }
}