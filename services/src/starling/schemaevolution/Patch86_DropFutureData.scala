package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import starling.daterange.Day
import starling.schemaevolution.system.Patch
import starling.services.StarlingInit

class Patch86_DropFutureData extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    starling.inTransaction {
      newWriter => {
        writer.update("delete from MarketData where observationDay > :day", Map("day"->Day(2011, 6, 1)))
      }
    }
  }

  def patchDescription = "Deletes blank data in Nov 2011"
}