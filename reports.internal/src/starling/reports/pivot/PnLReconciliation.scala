package starling.reports.pivot

import starling.pivot._
import starling.daterange.Timestamp
import starling.utils.CollectionUtils
import starling.dbx.QueryBuilder._
import starling.tradestore.eai.EAITradeAttributes
import starling.quantity.{Quantity, UOM}
import starling.db.{DB}
import starling.instrument.Trade
import starling.tradestore.{TradeSet, TradeAndFields}
import starling.reports.internal.AbstractReportContext

case class PnLQuantity(starling: Option[PivotQuantity], aspect: Option[PivotQuantity]) {

  def +(other: PnLQuantity) = {
    val newStarling = add(starling, other.starling)
    val newAspect = add(aspect, other.aspect)
    new PnLQuantity(newStarling, newAspect)
  }

  private def add(a: Option[PivotQuantity], b: Option[PivotQuantity]) = (a, b) match {
    case (Some(first), None) => Some(first)
    case (None, Some(second)) => Some(second)
    case (Some(first), Some(second)) => Some(first + second)
    case _ => None
  }

  def value: PivotQuantity = {
    (starling, aspect) match {
      case (Some(s), None) => s
      case (None, Some(a)) => a
      case (Some(s), Some(a)) => s - a
      case _ => PivotQuantity.NULL
    }
  }
}

class PnLFieldDetails(field: Field) extends FieldDetails(field) {

  override def combineGroup(groupA: Any, groupB: Any) = (groupA, groupB: @unchecked) match {
    case (g1: PnLQuantity, g2: PnLQuantity) => g1 + g2
  }

  override def nullGroup() = PnLQuantity(None, None)

  override def nullValue() = PnLQuantity(None, None)

  override def combine(group: Any, value: Any) = (group, value: @unchecked) match {
    case (g: PnLQuantity, v: PnLQuantity) => g + v
  }

  override def value(a: Any) = (a: @unchecked) match {
    case (v: PnLQuantity) => v.value
  }
  override def formatter = StandardPivotQuantityFormatter
}

class PnLReconciliation(reportContext: AbstractReportContext, tradeSet: TradeSet, timestamp: Timestamp, db: DB) extends UnfilteredPivotTableDataSource {
  lazy val (fieldDetailsGroup, trades) = tradeSet.readAll(timestamp, Some(reportContext.marketDayAndTime.day))
  lazy val book = trades.map(_.trade.attributes match {
    case a: EAITradeAttributes => a.bookID.id
  }).distinct.head

  val systemField = Field("System")
  val pnlField = Field("PnL")
  val pnlFieldDetails = new PnLFieldDetails(pnlField)

  override def initialState = {
    new PivotFieldsState(
      columns =
        ColumnTrees(List(ColumnTree(pnlField, true)))
    )
  }

  override def drillDownGroups = {
    List(
      DrillDownInfo(PivotAxis(List(pnlField), List(), List(systemField), false)),
      DrillDownInfo(PivotAxis(List(), List(Field("Instrument")), List(), false)),
      DrillDownInfo(PivotAxis(List(), List(Field("Market")), List(), false)),
      DrillDownInfo(PivotAxis(List(), List(Field("Period")), List(), false)),
      DrillDownInfo(PivotAxis(List(), List(Field("Trade ID")), List(), false))
    )
  }

  val fieldDetailsGroups = {
     FieldDetailsGroup("Reconciliation", List(FieldDetails(systemField), pnlFieldDetails)) :: fieldDetailsGroup
  }

  private val fieldsByName = Map() ++ fieldDetails.map(f => f.field.name -> f.field)

  lazy val aspectTrades = {
    val dealIDs = trades.map(_.trade.attributes).map{
      case attributes: EAITradeAttributes => {
        attributes.dealID.id
      }
    }

    val max = db.queryWithOneResult("select max(id) as m from tblDownloads where bookid = " + book) {
      rs => rs.getInt("m")
    }

    db.queryWithResult((
      select("aspectexternalid, totalAct, totalMtm")
        from ("tblaspectpnlobjects")
        where (
        ("downloadid" eql max.get) and
        ("quantityBBLs" isNotNull) and
          ("dealID" in dealIDs) and
          ("plutotradeid" isNull)
        )
      )) {
      rs => {
        val pnl = rs.getDouble("totalMtm") - rs.getDouble("totalAct")
        (rs.getString("aspectexternalid") -> pnl)
      }
    }.toMap
  }

  def unfilteredData(pfs: PivotFieldsState): List[Map[Field, Any]] = {
    val env = reportContext.environment
    val rows = trades.flatMap{
      case TradeAndFields(_, trade: Trade, tradeFields) => {
        val mtm = try {
          val t = trade.copy(costs = Nil) // aspect has no costs in the pnl table
          PivotQuantity(t.mtm(env, UOM.USD))
        } catch {
          case e => {
            println("ex: " + e)
            new PivotQuantity(e)
          }
        }
        val aspectMtm = try {
          PivotQuantity(Quantity(aspectTrades(trade.tradeID.toString), UOM.USD))
        } catch {
          case e => new PivotQuantity(new Exception("No trade with that id in Aspect: " + trade.tradeID, e))
        }
        //Ensure that if one failed they both fail. Otherwise the totals are nonsense
        val (sMtm, aMtm) = (mtm.hasErrors, aspectMtm.hasErrors) match {
          case (false, false) => (mtm, aspectMtm)
          case (true, false) => (mtm, new PivotQuantity(new Exception("Starling mtm failed")))
          case (false, true) => (new PivotQuantity(new Exception("Aspect mtm retreval failed")), aspectMtm)
          case (true, true) => (mtm, aspectMtm)
        }

        List(
          Map(systemField -> "Starling", pnlField -> PnLQuantity(Some(sMtm), None)) ++ tradeFields,
          Map(systemField -> "Aspect", pnlField -> PnLQuantity(None, Some(aMtm))) ++ tradeFields
        )
      }
    }
    rows
  }
}