package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchUtils, Patch}

class Patch119_AddEvenMoreColumnsToTitanTradeTable extends Patch {
  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch119_AddEvenMoreColumnsToTitanTradeTable.sql"))
  }
}

