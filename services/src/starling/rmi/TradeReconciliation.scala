package starling.rmi

import starling.services.trade.TradeDiff
import starling.pivot._
import starling.gui.api.{TradeSystemLabel, TradeIDLabel}
import starling.tradestore.TradeAndFields
import starling.tradestore.intraday.IntradayTradeAttributes

class TradeReconciliation(tradeDiff: TradeDiff) extends UnfilteredPivotTableDataSource {
  val realTradeAttributeKeys = List()//(tradeAttributeKeys filterNot (List("Deal", "Strategy", "Book") contains)) ::: List("DealID", "BookID", "StrategyID")
  
  val fieldsToExcludeFromInitialState = List("Trade ID", "Maturity Day", "Delivery Day", "RIC", "Error", "Estimated Delivery", "Fixations",
    "Timestamp", "Spread", "Trader", "Traded For", "Broker", IntradayTradeAttributes.subgroupName_str, "Trade Count", "Entry Date", "Username")


  override def initialState = {
    val measures = tradeDiff.fields.flatMap(_.fields.map(_.field)).filterNot { f => fieldsToExcludeFromInitialState.contains(f.name) }

    PivotFieldsState(
      rowFields = List(reconciliationField, groupField, Field("Trade ID")),
      dataFields = measures
    )
  }

  val reconciliationField = Field("Reconciliation")
  val groupField = Field("Group")

  val fieldDetailsGroups = {
    FieldDetailsGroup("Reconciliation Fields", FieldDetails(reconciliationField)) ::
    FieldDetailsGroup("Group Fields", FieldDetails(groupField)) ::
    tradeDiff.fields
  }

  private val fieldsByName = Map() ++ fieldDetails.map(f => f.field.name -> f.field)


  def unfilteredData(pfs: PivotFieldsState) = {
    var group = 0

    def buildRow(reconciliationType: String, trades: (TradeAndFields, TradeAndFields)): Seq[Map[Field, Any]] = {
      group += 1
      val t1 = trades._1
      val t2 = trades._2
      List(
        Map(reconciliationField -> reconciliationType, groupField -> group) ++ t1.fields,
        Map(reconciliationField -> reconciliationType, groupField -> group) ++ t2.fields
      )
    }

    def buildRowSingle(reconciliationType: String, trade: TradeAndFields): Map[Field, Any] = {
      group += 1
      Map(reconciliationField -> reconciliationType, groupField -> group) ++ trade.fields
    }

    val resultData = tradeDiff.matches.flatMap(ts => buildRow("Matches", ts)) :::
            tradeDiff.nearMatches.flatMap(ts => buildRow("Near Matches", ts)) :::
            tradeDiff.unmatched.map(t => buildRowSingle("Unmatched", t))

    resultData
  }
}