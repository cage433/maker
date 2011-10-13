package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.tradestore.eai.EAITradeStore

class Patch135_FixClearportRounding extends Patch {

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    starling.inTransaction {
      writer =>
        writer.update("update markets set clearportprecision='None' where clearportprecision='Some(0)'")
    }
  }

}
