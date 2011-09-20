package starling.trades.internal

import starling.tradestore.{TradeAndFields, TradeChanges}
import starling.pivot._

class TradeChangesPivotTableDataSource(tradeChanges: TradeChanges) extends UnfilteredPivotTableDataSource {
  override def initialState = {
    val columns = fieldDetails.map(_.field).filter(f => f.name != "Trade ID" && f.name != "Action").map(field => ColumnTree(field, true))
    new PivotFieldsState(
      rowFields = List(Field("Trade ID"), actionField),
      columns = ColumnTrees(columns)
    )
  }

  val actionField = Field("Action")

  val fieldDetailsGroups = FieldDetailsGroup("Action Fields", List(FieldDetails(actionField))) :: tradeChanges.fields

  private val fieldsByName = Map() ++ fieldDetails.map(f => f.field.name -> f.field)

  def unfilteredData(pfs: PivotFieldsState) = {
    def buildRow(action: String, trade: TradeAndFields) = {
      Map(actionField -> action) ++ trade.fields
    }
    val resultData =
    tradeChanges.movedIn.map(trade => buildRow("Moved In", trade)) :::
            tradeChanges.movedOut.map(trade => buildRow("Moved Out", trade)) :::
            tradeChanges.created.map(trade => buildRow("Created", trade)) :::
            tradeChanges.deleted.map(trade => buildRow("Deleted", trade)) :::
            tradeChanges.undeleted.map(trade => buildRow("Undeleted", trade)) :::
            tradeChanges.amended.flatMap {case (t1, t2) => List(buildRow("Amended From", t1), buildRow("Amended To", t2))}
    resultData
  }
}


