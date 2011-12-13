package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch156_ChangeSHFELeadLimSymbol extends Patch {
  protected def runPatch(ignore: StarlingInit, starling: RichDB, writer: DBWriter) =
    writer.update("UPDATE Markets SET limSymbol = 'Some(PBL)' WHERE name = 'Shanghai Lead' AND limSymbol = 'Some(PB)'")

  private def reversePatch(writer: DBWriter) =
    writer.update("UPDATE Markets SET limSymbol = 'Some(PB)' WHERE name = 'Shanghai Lead' AND limSymbol = 'Some(PBL)'")
}