package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch78_ObservationTime extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("alter table MarketData add observationTime varchar(60) not null default('Default')")
  }

  def patchDescription = "ObservationTime"
}