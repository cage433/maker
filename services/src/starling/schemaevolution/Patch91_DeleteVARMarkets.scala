package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch91_DeleteVARMarkets extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val deletedMarkets =
      List("Shanghai Fuel Oil", "Shanghai Gold London Close", "Shanghai Natural Rubber",
        "Shanghai Zinc London Close", "Shanghai Aluminium London Close", "Shanghai Copper London Close")
    deletedMarkets.foreach { market =>
      writer.update("delete from MarketData where marketDataKey like '%" + market + "%'")
    }
    writer.update("delete from MarketData where marketDataType = '<starling.marketdata.CurveIDDataType_-/>'")
  }
}