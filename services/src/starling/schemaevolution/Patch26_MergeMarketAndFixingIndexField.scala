package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch26_MergeMarketAndFixingIndexField extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    for (table <- List("TrinityTrade", "GalenaTrade", "EAITrade", "IntradayTrades", "Instrument")) {
      val indexes = starling.queryWithResult("select distinct fixingIndex from " + table, Map()) { rs=>rs.getString("fixingIndex") }
      for (index <- indexes) {
        writer.update(
          "update " + table + " set market = :index where fixingIndex = :index", Map("index"->index)
        )
      }
      writer.update("ALTER TABLE " + table + " drop column fixingIndex")
    }
  }

  def patchDescription = "Merge fixingIndex field into market field"
}