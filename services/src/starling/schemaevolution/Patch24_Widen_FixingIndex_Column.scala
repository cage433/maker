package starling.schemaevolution


import system.Patch
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.services.StarlingInit

class Patch24_Widen_FixingIndex_Column extends Patch{
   protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
     for (table <- List("Trinity", "Galena", "EAI")) {
       writer.update("ALTER TABLE dbo." + table + "Trade alter column [fixingIndex] varchar(255)")
     }
   }
  def patchDescription = "Widen the *Trade fixingIndex column to 255 chars."

}