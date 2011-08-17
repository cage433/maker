package starling.gui.pages

import starling.daterange.Timestamp
import starling.gui.StarlingServerContext
import starling.gui.api.{IntradayUpdated, TradeTimestamp, TradeSelection}
import collection.mutable.ListBuffer
import swing.event.Event
import starling.pivot.PivotEdits
import starling.browser.Page

case class TradeReconciliationReportPage(tradeSelection:TradeSelection, from:TradeTimestamp, to:TradeTimestamp,
                                         intradayTimestamp: Timestamp, pivotPageState:PivotPageState) extends AbstractPivotPage(pivotPageState) {
  def text = "Trade Reconciliation"
  override def layoutType = Some("TradeReconciliation")

  def selfPage(pivotPageState: PivotPageState, edits:PivotEdits) = copy(pivotPageState = pivotPageState)

  def dataRequest(pageBuildingContext:StarlingServerContext) = {
    pageBuildingContext.cachingStarlingServer.tradeReconciliation(tradeSelection, from, to, intradayTimestamp, pivotPageState.pivotFieldParams)
  }

  override def refreshFunctions = {
    val functions = new ListBuffer[PartialFunction[Event, Page]]
    tradeSelection.intradaySubgroup match {
      case Some(groups) => functions += {
        case IntradayUpdated(group, _, timestamp) if groups.subgroups.contains(group) => {
          this.copy(intradayTimestamp = timestamp)
        }
      }
      case _ =>
    }
    functions.toList
  }
}