package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


/**
 * Had bug that created a mess of identical trade versions - given this isn't in production it is far easier to delete all
 * versions but latest.
 */
class Patch131_PurgeExcessTitanTrades extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
     writer.update("delete from titantrade where timestampTo_Cache is not null")
  }
}