package starling.neptune

import starling.utils.Broadcaster
import java.lang.String
import starling.richdb.{RichInstrumentResultSetRow, RichDB}
import starling.eai.TreeID
import starling.db.RefinedAssignmentTradeSystem
import starling.market.NeptunePricingExchange
import starling.utils.ImplicitConversions._
import starling.tradestore.TradeStore
import starling.trade.TradeAttributes
import starling.pivot._
import starling.pivot.Field._
import starling.instrument.TradeableType

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
  def details = Map(
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



class RefinedAssignmentTradeStore(db: RichDB, broadcaster:Broadcaster)
  extends TradeStore(db, broadcaster, RefinedAssignmentTradeSystem, None, false)
{
  val tradeAttributesFactory = RefinedAssignmentTradeAttributes

  def pivotDrillDownGroups() = {
    List(
      DrillDownInfo(PivotAxis( List(), List(Field("Hub")), List(), false)),
      DrillDownInfo(PivotAxis( List(), List(Field("Pricing Type"), Field("Commodity Category")), List(), false)),
      instrumentFilteredDrillDown
    )
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
    PivotFieldsState(
        List(Field("Trade Count"))
      )
  }

  val tableName = "RefinedAssignment"
}
