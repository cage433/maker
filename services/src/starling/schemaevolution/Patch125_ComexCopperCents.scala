package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch125_ComexCopperCents extends Patch {
  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    writer.update("update Markets set limMultiplier = null, ccy = 'USC' where Name = 'COMEX High Grade Copper'")
  }
}
