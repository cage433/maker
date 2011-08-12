package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch109_RenameCFDs extends Patch {
  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    val sql = "update EAITrade set instrument = 'Commodity Swap', cleared = 1, PricingRule = 'Common' where instrument = 'CFD'"
    writer.update(sql)
  }
}