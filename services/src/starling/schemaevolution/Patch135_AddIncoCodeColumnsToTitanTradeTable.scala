package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch135_AddIncoCodeColumnsToTitanTradeTable extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(
    """
      truncate table TitanTrade
      alter table TitanTrade add contractIncoTermCode varchar(100)
      alter table TitanTrade add benchmarkIncoTermCode varchar(100) NULL
    """
    )
  }
}