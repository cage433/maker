package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch132_RenameTitanTradeColumns extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("truncate table TitanTrade")
    writer.update("exec sp_rename 'TitanTrade.[benchmarkDeliveryLocation]', 'benchmarkCountryCode', 'column'")
    writer.update("exec sp_rename 'TitanTrade.[contractDeliveryLocation]', 'contractLocationCode', 'column'")
    writer.update("exec sp_rename 'TitanTrade.[grade]', 'gradeCode', 'column'")
  }
}