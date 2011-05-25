package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch69_RenameStrikeToPrice extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val instruments = "('Future', 'Commodity Swap', 'Commodity Forward', 'FX Forward', 'Forward Freight Agreement', 'Futures Spread')"
    system.PatchUtils.foreachTradeTable { table => {
      if (table != "RefinedAssignment" && table != "RefinedFixation") {
        writer.update("alter table " + table + " add InitialPrice varchar(50)")
        writer.update("alter table " + table + " add InitialPriceUOM varchar(20)")
        writer.update("update " + table + " set InitialPrice = strike, InitialPriceUOM = strikeUOM, strike = null, strikeUOM=null where instrument in " + instruments)
      }
    } }
  }

  def patchDescription = "Rename Volume column to Quantity"
}