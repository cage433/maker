package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{PatchUtils, Patch}
import starling.services.StarlingInit

class Patch30_RemoveZZZM_ReportingEntity extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch30_RemoveZZZM_ReportingEntity.sql"))
  }

  def patchDescription = "Remove ZZZM reporting entity"
}