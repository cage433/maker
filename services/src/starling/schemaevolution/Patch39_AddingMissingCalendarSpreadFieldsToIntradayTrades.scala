package starling.schemaevolution

import system.Patch
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.services.StarlingInit

class Patch39_AddingMissingCalendarSpreadFieldsToIntradayTrades extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("""
    alter table IntradayTrades add
      firstSpreadPeriod varchar(255),
      secondSpreadPeriod varchar(255)""")
  }

  def patchDescription = "The intraday trade tables didn't have these fields"
}
