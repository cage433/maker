package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{PatchUtils, Patch}
import starling.services.StarlingInit

class Patch74_UpdatePivotLayoutsWithTotals extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling:RichDB, writer:DBWriter) = {
    writer.update("alter table PivotLayouts add otherLayoutInfo text")
  }

  def patchDescription = "Adds a new column to the PivotLayout table to store the totals state"
}