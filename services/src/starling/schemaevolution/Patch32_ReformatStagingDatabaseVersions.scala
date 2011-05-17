package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{PatchUtils, Patch}
import starling.services.StarlingInit

class Patch32_ReformatStagingDatabaseVersions extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    starling.inTransaction(innerWriter => {
      innerWriter.update("alter table dbo.ForwardCurveStaging add newVersion int identity(0, 1)")
    })
    starling.inTransaction(innerWriter => {
      innerWriter.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch32_ReformatStagingDatabaseVersions.sql"))
    })
    starling.inTransaction(innerWriter => {
      innerWriter.update("exec sp_RENAME @objname = 'dbo.ForwardCurveStaging.newVersion', @newname = 'version', @objtype = 'COLUMN'")
    })
    starling.inTransaction(innerWriter => {
      innerWriter.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch32_ReformatStagingDatabaseVersions2.sql"))
    })
  }

  def patchDescription = "Reformat version numbers in FC staging database."
}