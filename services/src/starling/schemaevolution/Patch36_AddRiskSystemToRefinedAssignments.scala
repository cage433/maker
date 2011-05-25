package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{PatchUtils, Patch}
import starling.services.StarlingInit

class Patch36_AddRiskSystemToRefinedAssignments extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch36_AddRiskSystemToRefinedAssignments.sql"))
  }

  def patchDescription = "Add riskArea column to RefinedAssignment table"
}