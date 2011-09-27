package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter

class Patch124_ComexCopperLIMMultiplier extends Patch {
  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    writer.update("update Markets set limMultiplier = 'Some(0.01)' where Name = 'COMEX High Grade Copper'")
  }
}
