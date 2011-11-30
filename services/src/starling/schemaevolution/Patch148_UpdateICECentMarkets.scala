package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.gui.api.{EAIDeskInfo, Desk}


class Patch148_UpdateICECentMarkets extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    writer.update("update markets set ccy = 'USC' where name in ('ICE Heat', 'ICE RBOB')")
  }

}
