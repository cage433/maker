package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch142_TruncateTitanTradesAsFixedPricingSpecHasChanged extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(
      "truncate table TitanTrade"
    )
  }
}
