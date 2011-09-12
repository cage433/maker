package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchUtils, Patch}
import starling.market.MarketProvider

class Patch115_UpdateMarkets1 extends Patch {
  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    starling.inTransaction{
      writer => {
        writer.update("update markets set name = 'nymex rbob 1st month vs ipe brent 1st month (bbls)' where name = 'nymex rbob 1st month vs ipe brent 1st month'")
        writer.update("update intradaytrades set market = 'nymex rbob 1st month vs ipe brent 1st month (bbls)' where market = 'nymex rbob 1st month vs ipe brent 1st month'")
        writer.update("update eaitrade set market = 'nymex rbob 1st month vs ipe brent 1st month (bbls)' where market = 'nymex rbob 1st month vs ipe brent 1st month'")
      }
    }
    MarketProvider.reload
  }
}