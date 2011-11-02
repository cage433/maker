package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchUtils, Patch}


class Patch140_UpdateEmailSnapshotTypeToMarketData extends Patch {
  protected def runPatch(init: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch140_UpdateEmailSnapshotTypeToMarketData.sql"))
  }
}