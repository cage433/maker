package starling.schemaevolution

import starling.db.DBWriter
import system.Patch
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch31_AddEquitiesColumn extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("alter table IntradayTrades add ric varchar(255)")
    writer.update("alter table Instrument add ric varchar(255)")
  }

  def patchDescription = "Adds the ric field to IntrdayTrade and Instrument"
}