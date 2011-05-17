package starling.schemaevolution

import starling.richdb.{RichDB}
import system.{Patch, PatchUtils}
import starling.db.{RevalSnapshotDB, DBWriter}
import starling.services.StarlingInit

/**
 * Creates the revalsnapshot table
 */
class Patch8_RevalSnapshotDB extends Patch {
  private def floatFields = List(
    "REVAL_NPV", "REVAL_REALISED", "REVAL_VALUE",
    "REVAL_COST", "REVAL_COMMISSION", "POSITION_FX_SPT", "POSITION_FX_FWD", "POSITION_FX_FUT",
    "IR_POSITION", "MODIFIED_DURATION", "CONVEXITY", "PRICE_UNIT_VALUE", "PRICE_FORWARD",
    "PRICE_SPOT", "SPOT_VAL_PER_DEAL", "MATURITY_DF", "DELTA", "GAMMA_SPT", "GAMMA_FWD", "GAMMA_FUT",
    "VOLATILITY", "THETA", "VEGA", "REVAL_NFV", "REVAL_VALUE_FWD", "REVAL_COST_FWD", "REVAL_COMMISSION_FWD",
    "PHYS_PREMIUM")

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    PatchUtils.executeUpdateSQL(
      writer,
      "create table dbo.RevalSnapshot(snapshotid int, tradeid int, " + floatFields.mkString(" float, ") +
              " float, reval_curr char(3), position_curr char(3), " +
              " COMMISSION_IN_COST char(1), " +
              " timestamp datetime," +
              " CONSTRAINT PK_revalsnapshot PRIMARY KEY CLUSTERED (snapshotid ASC, tradeid ASC) ON [PRIMARY])"
     )
  }
}