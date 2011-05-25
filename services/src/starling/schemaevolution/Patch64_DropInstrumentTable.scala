package starling.schemaevolution

import starling.db.DBWriter
import system.Patch
import starling.richdb.RichDB
import starling.services.StarlingInit

/**
 * Created by IntelliJ IDEA.
 * User: thomas
 * Date: 15/12/10
 * Time: 13:35
 * To change this template use File | Settings | File Templates.
 */

class Patch64_DropInstrumentTable  extends Patch {

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {

    writer.update("drop table Instrument")
    writer.update("drop index TrinityTrade.NUC_TrinityTrade")
    writer.update("drop index GalenaTrade.NUC_GalenaTrade")
    system.PatchUtils.foreachTradeTable { table => {
      writer.update("drop table " + table + "UTP")
      //writer.update("alter table " + table + " drop column timestampTo_cache")
      //writer.update("alter table " + table + " drop column nextVersionId_cache")
    }}
  }

  def patchDescription = "Drop Instrument *UTP tables and cache fields"
}