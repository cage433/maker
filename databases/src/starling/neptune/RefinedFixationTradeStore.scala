package starling.neptune

import starling.utils.Broadcaster
import java.lang.String
import starling.richdb.{RichInstrumentResultSetRow, RichDB}
import starling.eai.TreeID
import starling.db.RefinedFixationTradeSystem
import starling.market.NeptunePricingExchange
import starling.utils.ImplicitConversions._
import starling.tradestore.{TradeStore}
import starling.pivot._
import starling.instrument.TradeAttributes

import starling.pivot.Field._
import starling.instrument.{TradeableType}
import collection.immutable.TreeMap


case class RefinedFixationTradeAttributes (
  groupCompany : String,
  exchange : String,
  contractNo : String,
  pricingType : String
)
  extends TradeAttributes
{
  def details = Map(
    groupCompany_str -> groupCompany,
    exchange_str -> exchange,
    contractNo_str -> contractNo,
    pricingType_str -> pricingType
  )
}


object RefinedFixationTradeStore {
}

class RefinedFixationTradeStore(db: RichDB, broadcaster:Broadcaster)
  extends TradeStore(db, broadcaster, RefinedFixationTradeSystem, None)
{

  override val tradeAttributeFieldDetails = List(groupCompany_str, exchange_str, contractNo_str, pricingType_str).map(n=>FieldDetails(n))
  private val List(groupCompany_col, exchange_col, contractNo_col, pricingType_col) = tradeAttributeFieldsAsSQLColumnNames

  def createTradeAttributes(row: RichInstrumentResultSetRow) = {

    RefinedFixationTradeAttributes(
      row.getString(groupCompany_col),
      row.getString(exchange_col),
      row.getString(contractNo_col),
      row.getString(pricingType_col)
    )

  }

  def pivotDrillDownGroups() = List()

  def pivotInitialState(tradeableTypes:Set[TradeableType[_]]) = {
    PivotFieldsState(
        List(Field("Trade Count"))
      )
  }

  val tableName = "RefinedFixation"
}
