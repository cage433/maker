package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.utils.ImplicitConversions._
import starling.services.StarlingInit

class Patch48_RemoveUnneededOptionTypes extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    List("EAITrade", "GalenaTrade", "TrinityTrade", "IntradayTrades", "Instrument").foreach{
      table =>
        writer.update("exec sp_rename '[%s].optionType', 'exerciseType', 'COLUMN'" % table)
        writer.update(
          """
            update %s
             set exerciseType = null
             where exerciseType = 'Calendar Spread' or exerciseType = 'Asian'
          """ % table
        )
    }
    
  }

  def patchDescription = "Option type becomes exercise type and Amer/Eur only"
}