package starling.gui.pages

import starling.gui.{Page, PageContext, PageBuildingContext}
import swing.Reactions
import starling.daterange.{Day, Timestamp}
import starling.gui.api._
import starling.pivot.{SomeSelection, Field, Selection}
import starling.rmi.StarlingServer
import collection.mutable.ListBuffer
import swing.event.Event

class PnlPivotReportPage

case class TradeChangesReportPage(tradeSelection:TradeSelection, from:TradeTimestamp, to:TradeTimestamp,
                                  pivotPageState:PivotPageState, tradeExpiryDay:Day) extends AbstractPivotPage(pivotPageState) {
  assert(tradeSelection.intradaySubgroup.isEmpty, "Trade changes report doesn't support excel trades")

  def dataRequest(pageBuildingContext:PageBuildingContext) = {
    pageBuildingContext.cachingStarlingServer.tradeChanges(tradeSelection, from.timestamp, to.timestamp, from.timestamp.day.startOfFinancialYear, pivotPageState.pivotFieldParams)
  }

  override def finalDrillDownPage(fields:Seq[(Field,Selection)], pageContext:PageContext, ctrlDown:Boolean):Unit = {
    val selection = fields.find(f=>f._1.name == "Trade ID")
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
        pageContext.createAndGoTo(
          (starlingServer:StarlingServer) => {
            SingleTradePage(trID, tradeSelection.desk, TradeExpiryDay(tradeExpiryDay), tradeSelection.intradaySubgroup)
          }, newTab = ctrlDown)
      }
      case None => None
    }
  }

  def text = "Trade Changes " + tradeSelection + " " + from + " to " + to
  override def layoutType = Some("TradeChanges")
  def selfPage(pivotPageState:PivotPageState) = copy(pivotPageState=pivotPageState)

  override def refreshFunctions = {
    val functions = new ListBuffer[PartialFunction[Event,Page]]
    functions.toList
  }
}