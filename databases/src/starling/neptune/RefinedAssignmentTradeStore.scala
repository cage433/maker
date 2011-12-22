package starling.neptune

import java.lang.String
import starling.richdb.{RichInstrumentResultSetRow, RichDB}
import starling.eai.TreeID
import starling.db.RefinedAssignmentTradeSystem
import starling.market.FuturesExchange
import starling.utils.ImplicitConversions._
import starling.instrument.TradeAttributes
import starling.pivot._
import starling.pivot.Field._
import starling.instrument.TradeableType
import starling.gui.api.Desk
import starling.manager.Broadcaster
import starling.tradeimport.ClosedDesks
import starling.tradestore.{RichTradeStore, TradeStore}
import starling.daterange.Timestamp

case class RefinedAssignmentTradeAttributes(
  groupCompany : String,
  exchange : String,
  hub : String,
  commodityCategory : String,
  contractNo : String,
  allocationNo : String,
  riskArea : String
)
  extends TradeAttributes
{
  def persistedDetails = Map(
    groupCompany_str -> groupCompany,
    exchange_str -> exchange,
    hub_str -> hub,
    commodityCategory_str -> commodityCategory,
    contractNo_str -> contractNo,
    allocationNo_str -> allocationNo,
    riskArea_str -> riskArea
  )
}


object RefinedAssignmentTradeAttributes {

}



class RefinedAssignmentTradeStore(db: RichDB, broadcaster:Broadcaster, closedDesks: ClosedDesks)
  extends RichTradeStore(db, RefinedAssignmentTradeSystem, closedDesks: ClosedDesks)
{
  val tradeAttributesFactory = RefinedAssignmentTradeAttributes
  def deskOption = Some(Desk.Titan)

  def pivotDrillDownGroups() = {
    List(
      DrillDownInfo(PivotAxis( List(), List(Field("Hub")), List(), false)),
      DrillDownInfo(PivotAxis( List(), List(Field("Pricing Type"), Field("Commodity Category")), List(), false)),
      instrumentFilteredDrillDown
    )
  }

  protected def closesFrom(from:Timestamp, to:Timestamp) = {
    (to :: allTimestamps).distinct.filter(t => t > from && t <= to).sortWith(_ < _)
  }

  override val tradeAttributeFieldDetails = List(groupCompany_str, exchange_str, hub_str, commodityCategory_str, contractNo_str,
    allocationNo_str, riskArea_str).map(n=>FieldDetails(n))

  private val List(groupCompany_col, exchange_col, hub_col, commodityCategory_col, contractNo_col,
    allocationNo_col, riskArea_col) = tradeAttributeFieldsAsSQLColumnNames

  def createTradeAttributes(row: RichInstrumentResultSetRow) = {

    RefinedAssignmentTradeAttributes(
      row.getString(groupCompany_col),
      row.getString(exchange_col),
      row.getString(hub_col),
      row.getStringOrBlank(commodityCategory_col),
      row.getString(contractNo_col),
      row.getStringOrBlank(allocationNo_col),
      row.getStringOrBlank(riskArea_col)
    )
  }
//  def pivotDrillDownGroups() = List()

  def pivotInitialState(tradeableTypes:Set[TradeableType[_]]) = {
    val pfs = PivotFieldsState(
        List(Field("Trade Count"))
      )
    DefaultPivotState(pfs)
  }

  val tableName = "RefinedAssignment"
}
