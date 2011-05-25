package starling.db

import starling.utils.StarlingTest
import org.springframework.jdbc.datasource.SingleConnectionDataSource

import org.testng.Assert._
import starling.quantity.{UOM, Quantity}
import org.testng.annotations.{BeforeTest, Test}

/**
 * Tests a snapshot. Just tests the reval_value and delta filter and checks that a single field gets through
 */
class RevalSnapshotDBTest extends DBTest {

  @Test
  def testSimpleSnapshot {
    val rows = List[Map[String,Any]](
      Map("profileid"->1, "identid"->12, "reval_curr"->"USD", "reval_npv"->20, "maturity_df"->1, "delta"->1), //prifileid 1 should be ignored
      Map("profileid"->0, "identid"->13, "reval_curr"->"USD", "reval_npv"->0, "maturity_df"->1, "delta"->0), //no value or delta
      Map("profileid"->0, "identid"->14, "reval_curr"->"USD", "reval_npv"->0, "maturity_df"->1, "delta"->1), //just a value
      Map("profileid"->0, "identid"->15, "reval_curr"->"USD", "reval_npv"->20, "maturity_df"->1, "delta"->0), //just a delta
      Map("profileid"->0, "identid"->16, "reval_curr"->"USD", "reval_npv"->23, "maturity_df"->1, "delta"->1)
      )
    inPopulatedBradyDatabase(rows,
      bradyDB => {
        inEmptyRevalSnapshotDB {
          revalSnapshotDB => {
            revalSnapshotDB.snapshot(100, bradyDB)
            val reval = revalSnapshotDB.readSnapshot(100)
            assertEquals(reval.data.size, 3)
            assertEquals(reval(0, "Trade ID"), "ga14")
            assertEquals(reval(2, "Reval NPV"), Quantity(23.0, UOM.USD))
          }
        }
      })
  }

  private def inEmptyRevalSnapshotDB(f:(RevalSnapshotDB) => Unit) = {
    val connection = getConnection("jdbc:derby:memory:revalsnpashottest;create=true");
    val ds = new SingleConnectionDataSource(connection, true)
    val db = new DB(ds)
    db.inTransaction {
      writer => {
        writer.update(revalSnapshot_CreateTableSQL)
        try {
          f(new RevalSnapshotDB(db))
        } finally {
          writer.update("drop table RevalSnapshot")
        }
      }
    }
    connection.close
  }

  private def inPopulatedBradyDatabase(values:List[Map[String,Any]], f:(DB) => Unit) = {
    val connection = getConnection("jdbc:derby:memory:revalsnpashottestbrady;create=true");
    val ds = new SingleConnectionDataSource(connection, true)
    val db = new DB(ds)
    db.inTransaction {
      writer => {
        writer.update(brady_T_REVAL_TRADES_createTableSQL)
        writer.insert("T_REVAL_TRADES", values)
      }
    }
    try {
      f(db)
    } finally {
      db.inTransaction {
        writer => {
          writer.update("drop table T_REVAL_TRADES")
        }
      }
    }
    connection.close
  }

  private def brady_T_REVAL_TRADES_createTableSQL = {
    val floatFields = List(
      "REVAL_NPV", "REVAL_REALISED", "REVAL_VALUE",
      "REVAL_COST", "REVAL_COMMISSION", "POSITION_FX_SPT", "POSITION_FX_FWD", "POSITION_FX_FUT",
      "IR_POSITION", "MODIFIED_DURATION", "CONVEXITY", "PRICE_UNIT_VALUE", "PRICE_FORWARD",
      "PRICE_SPOT", "SPOT_VAL_PER_DEAL", "MATURITY_DF", "DELTA", "GAMMA_SPT", "GAMMA_FWD", "GAMMA_FUT",
      "VOLATILITY", "THETA", "VEGA", "REVAL_NFV", "REVAL_VALUE_FWD", "REVAL_COST_FWD", "REVAL_COMMISSION_FWD",
      "PHYS_PREMIUM")
    "create table T_REVAL_TRADES(profileid int, IDENTID int, " + floatFields.mkString(" float, ") +
            " float, reval_curr char(3), position_curr char(3), " +
            " COMMISSION_IN_COST char(1), " +
            " timestamp date)"
  }

  private def revalSnapshot_CreateTableSQL = {
      val floatFields = List(
        "REVAL_NPV", "REVAL_REALISED", "REVAL_VALUE",
        "REVAL_COST", "REVAL_COMMISSION", "POSITION_FX_SPT", "POSITION_FX_FWD", "POSITION_FX_FUT",
        "IR_POSITION", "MODIFIED_DURATION", "CONVEXITY", "PRICE_UNIT_VALUE", "PRICE_FORWARD",
        "PRICE_SPOT", "SPOT_VAL_PER_DEAL", "MATURITY_DF", "DELTA", "GAMMA_SPT", "GAMMA_FWD", "GAMMA_FUT",
        "VOLATILITY", "THETA", "VEGA", "REVAL_NFV", "REVAL_VALUE_FWD", "REVAL_COST_FWD", "REVAL_COMMISSION_FWD",
        "PHYS_PREMIUM")
      "create table RevalSnapshot(snapshotid int, tradeid int, " + floatFields.mkString(" float, ") +
              " float, reval_curr char(3), position_curr char(3), " +
              " COMMISSION_IN_COST char(1), " +
              " timestamp date)"

  }
}
