package starling.db

import starling.utils._
import starling.quantity.Quantity

/**
 * The class which wraps the RevalSnapshot table
 * This holds a copy of some of the values in the brady t_reval_trades table
 *
 * The t_reval_trades table holds the brady values for mtm, delta etc
 * It is read into starling to make starling-brady comparisons
 */
class RevalSnapshotDB(starlingDB:DB) {

  private def revalFields = List(
    "REVAL_CURR", "POSITION_CURR", "REVAL_NPV", "REVAL_REALISED", "REVAL_VALUE",
    "REVAL_COST", "REVAL_COMMISSION", "POSITION_FX_SPT", "POSITION_FX_FWD", "POSITION_FX_FUT",
    "IR_POSITION", "MODIFIED_DURATION", "CONVEXITY", "PRICE_UNIT_VALUE", "PRICE_FORWARD",
    "PRICE_SPOT", "SPOT_VAL_PER_DEAL", "MATURITY_DF", "DELTA", "GAMMA_SPT", "GAMMA_FWD", "GAMMA_FUT",
    "VOLATILITY", "THETA", "VEGA", "REVAL_NFV", "REVAL_VALUE_FWD", "REVAL_COST_FWD", "REVAL_COMMISSION_FWD",
    "COMMISSION_IN_COST", "PHYS_PREMIUM", "TIMESTAMP")
  def snapshot(snapshotID:Long, bradyDB:DB) {
    val buffer = new scala.collection.mutable.ArrayBuffer[Map[String,Any]]()
    starlingDB.inTransaction {
      dbWriter => {
        Log.infoWithTime("Reval snapshot") {
          import starling.dbx.QueryBuilder._
          val query =
          (select ("IDENTID as TRADEID, " + revalFields.mkString(", "))
           from ("T_REVAL_TRADES")
           where (("PROFILEID" eql 0) and (("REVAL_NPV" neq 0) or ("DELTA" neq 0)))
          )
          var count = 0
          bradyDB.query(query) {
            rs => {
              val parameters = Map("snapshotid"->snapshotID) ++ rs.asMap
              buffer.append(parameters)
              if (buffer.size > 1000) {
                dbWriter.insert("RevalSnapshot", buffer)
                count = count + buffer.size
                buffer.clear
              }
            }
          }
          dbWriter.insert("RevalSnapshot", buffer)
          count = count + buffer.size
          Log.info("Wrote " + count + " trade reval values")
        }
      }
    }
  }

  def tradeDetails(snapshotID:Long, tradeID:String):Option[STable] = {
    import starling.dbx.QueryBuilder._
    val query =
      (select ("*")
       from ("revalsnapshot")
       where (("SNAPSHOTID" eql snapshotID) and ("TRADEID" eql tradeID))
      )
    val table = new STable(
      "Reval",
      List(
        new SColumn("Reval NPV", QuantityColumnType),
        new SColumn("Delta", DoubleColumnType),
        new SColumn("Reval Cost", DoubleColumnType),
        new SColumn("Price Forward", DoubleColumnType),
        new SColumn("Volatility", DoubleColumnType),
        new SColumn("Maturity Discount Factor", DoubleColumnType),
        new SColumn("Undiscounted Mtm?", DoubleColumnType)),
      starlingDB.queryWithResult(query) {
        rs => List(
          new Quantity(rs.getDouble("REVAL_NPV"), rs.getUOM("REVAL_CURR")),
          rs.getDouble("DELTA"),
          rs.getDouble("REVAL_COST"),
          rs.getDouble("PRICE_FORWARD"),
          rs.getDouble("VOLATILITY"),
          rs.getDouble("MATURITY_DF"),
          (rs.getDouble("REVAL_NPV") / rs.getDouble("MATURITY_DF"))
        )
      }
    )
    if (table.data.length > 0) {
      Some(table)
    } else {
      None
    }
  }

  def readSnapshot(snapshotID:Long):STable = {
    import starling.dbx.QueryBuilder._
    val query =
      (select ("*")
       from ("REVALSNAPSHOT")
       where ("SNAPSHOTID" eql snapshotID)
      )
    STable(
      "Reval",
      List(
        SColumn("Trade ID"),
        new SColumn("Reval NPV", QuantityColumnType),
        new SColumn("Reval Delta", DoubleColumnType),
        new SColumn("Reval NPV undiscounted", QuantityColumnType)
      ),
      starlingDB.queryWithResult(query) {
        rs => List(
          "ga"+rs.getInt("TradeID"),
          new Quantity(rs.getDouble("REVAL_NPV"), rs.getUOM("REVAL_CURR")),
          rs.getDouble("DELTA"),
          new Quantity((rs.getDouble("REVAL_NPV") / rs.getDouble("MATURITY_DF")), rs.getUOM("REVAL_CURR"))
        )
      }
    )
  }

}