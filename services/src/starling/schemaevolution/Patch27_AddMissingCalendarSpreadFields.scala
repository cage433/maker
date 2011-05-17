package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch27_AddMissingCalendarSpreadFields extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("""
    alter table TrinityTrade add
      firstSpreadPeriod varchar(255),
      secondSpreadPeriod varchar(255)
            """)
    writer.update("""
    alter table GalenaTrade add
      firstSpreadPeriod varchar(255),
      secondSpreadPeriod varchar(255)
            """)
  }

  def patchDescription = "The brady trade tables didn't have these fields"
}