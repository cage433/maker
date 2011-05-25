package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch61_RenameVolumeToQuantity extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    List("EAITrade", "IntradayTrades", "SoftmarTrade", "RefinedAssignment","GalenaTrade", "TrinityTrade").foreach {
      table =>
        writer.update("exec sp_rename '" + table + ".volume', 'quantity', 'COLUMN'")
        writer.update("exec sp_rename '" + table + ".volumeUOM', 'quantityUOM', 'COLUMN'")
    }
  }

  def patchDescription = "Rename Volume column to Quantity"
}