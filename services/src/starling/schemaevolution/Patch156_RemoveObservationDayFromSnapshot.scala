package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch156_RemoveObservationDayFromSnapshot extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("alter table marketdatasnapshots drop column observationDay")
  }
}