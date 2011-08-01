package starling.schemaevolution

import system.Patch
import starling.calendar.BrentMonth
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter

class Patch109_FixLimCodes extends Patch {

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {

    val updates = List(
      "update Markets set limSymbol = 'Some(ZN)' where name = 'Shanghai Zinc'",
      "update Markets set limSymbol = 'Some(CU)' where name = 'Shanghai Copper'",
      "update Markets set limSymbol = 'Some(AL)' where name = 'Shanghai Aluminium'",
      "update Markets set limSymbol = 'Some(AU)' where name = 'Shanghai Gold'",
      "update Markets set limSymbol = 'Some(GC)' where name = 'COMEX Gold'",
      "update Markets set limSymbol = 'Some(HG)' where name = 'COMEX High Grade Copper'",
      "update Markets set limSymbol = 'Some(COMEX.PAC)' where name = 'COMEX Palladium'",
      "update Markets set limSymbol = 'Some(PL)' where name = 'COMEX Platinum'",
      "update Markets set limSymbol = 'Some(SI)' where name = 'COMEX Silver'")

    updates.foreach(writer.update)
  }
}