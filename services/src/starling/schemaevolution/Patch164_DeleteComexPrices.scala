package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchUtils, Patch}


class Patch164_DeleteComexPrices extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val patchUtil = MarketDataPatchUtil(starling, writer)
    patchUtil.deleteExtendedKeys("Price", "%COMEX%")
  }
}
