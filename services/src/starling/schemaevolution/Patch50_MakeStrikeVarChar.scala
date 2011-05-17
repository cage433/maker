package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch50_MakeStrikeVarChar extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    List("Instrument", "IntradayTrades", "EAITrade", "GalenaTrade", "SoftmarTrade", "TrinityTrade").foreach {
      table => {

        writer.update("alter table " + table + " alter column strike varchar(50)")
      }
    }
  }

  def patchDescription = "Turning strike column into varchar so that we can have K1/K2 format"
}