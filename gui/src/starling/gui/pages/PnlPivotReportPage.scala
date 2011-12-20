package starling.gui.pages

import starling.daterange.{Day, Timestamp}
import starling.gui.api._
import starling.rmi.StarlingServer
import collection.mutable.ListBuffer
import swing.event.Event
import starling.pivot.{PivotEdits, SomeSelection, Field, Selection}
import starling.gui.StarlingServerContext
import starling.browser.{Modifiers, Page, ServerContext, PageContext}
import starling.gui.StarlingLocalCache._

class PnlPivotReportPage

case class TradeChangesReportPage(tradeSelection:TradeSelection, from:TradeTimestamp, to:TradeTimestamp,
                                  pivotPageState:PivotPageState, tradeExpiryDay:Day) extends AbstractStarlingPivotPage(pivotPageState) {
  assert(tradeSelection.intradaySubgroup.isEmpty, "Trade changes report doesn't support excel trades")

  def dataRequest(pageBuildingContext:StarlingServerContext) = {
    pageBuildingContext.tradeService.tradeChanges(tradeSelection, from.timestamp, to.timestamp, from.timestamp.day.startOfFinancialYear, pivotPageState.pivotFieldParams)
  }

  override def finalDrillDownPage(fields:Seq[(Field,Selection)], pageContext:PageContext, modifiers:Modifiers, reportSpecificChoices : ReportSpecificChoices):Unit = {
    val selection = fields.find(f=>f._1.name == "Trade ID" && (f._2 match {
      case SomeSelection(vs) if vs.size == 1 => true
      case _ => false
    }))
    val tradeID = selection match {
      case Some( (field,selection)) => {
        selection match {
          case SomeSelection(values) if (values.size==1) => Some(values.toList(0).asInstanceOf[TradeIDLabel])
          case _ => None
        }
      }
      case None => None
    }
    tradeID match {
      case Some(trID) => {
        pageContext.goTo(
          SingleTradePage(trID, tradeSelection.desk.map(d => (d, pageContext.localCache.latestDeskTradeTimestamp(d))),
            TradeExpiryDay(tradeExpiryDay), tradeSelection.intradaySubgroup.map(g => (g, pageContext.localCache.latestTimestamp(g)))),
          modifiers = modifiers
        )
      }
      case None => None
    }
  }

  def text = "Trade Changes " + tradeSelection + " " + from + " to " + to
  override def shortText = "Trade Changes"
  def selfPage(pivotPageState:PivotPageState, edits:PivotEdits) = copy(pivotPageState=pivotPageState)
}