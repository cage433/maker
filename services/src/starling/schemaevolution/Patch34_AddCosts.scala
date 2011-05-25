package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch34_AddCosts extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    starling.inTransaction {
      newWriter => {
        val tradeTables = List("TrinityTrade", "GalenaTrade", "SoftmarTrade", "EAITrade", "RefinedAssignment", "RefinedFixation")

        for (table <- tradeTables) {
          writer.update("alter table " + table + " add costs text null")
        }

        writer.update("alter table instrument add cashInstrumentType varchar(50)")
      }
    }

    starling.inTransaction {
      newWriter => {
        newWriter.update("update Instrument set CashInstrumentType = 'General' where CashInstrumentType is null")
      }
    }
  }

  def patchDescription = "Add columns needed for Costs"
}