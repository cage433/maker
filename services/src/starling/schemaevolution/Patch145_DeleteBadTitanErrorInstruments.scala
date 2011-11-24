package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch145_DeleteBadTitanErrorInstruments extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("delete from TitanTrade where instrument = 'Error Instrument' and error is null")
  }
}