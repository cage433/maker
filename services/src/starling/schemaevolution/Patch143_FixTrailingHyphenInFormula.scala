package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch143_FixTrailingHyphenInFormula extends Patch {
  protected def runPatch(init: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("update Markets set formula = '1.0* MKT(1482)- 1.0* MKT(880)' where eaiQuoteID = 1762")
  }
}