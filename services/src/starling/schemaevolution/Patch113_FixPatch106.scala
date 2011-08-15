package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch113_FixPatch106 extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    writer.queryForUpdate("select * from marketdata where marketDAtaType = '<starling.curves.SpreadStdDevSurfaceDataType_-/>'") {
      rs => {
        val old = rs.getString("data")
        if (old != null) {
          val newS = old
                  .replaceAll("<first", "<front")
                  .replaceAll("<last", "<back")
          rs.update(Map("data" -> newS))
        }
      }
    }
  }
}