package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.Patch
import starling.market.MarketProvider

class Patch115_UpdateMarkets2 extends Patch {
  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    val names = List(
      ("HSFO 180 CST Singapore vs HSFO 380 CST Singapore", "HSFO 180 CST Singapore (6.5) vs HSFO 380 CST Singapore"),
      ("HSFO 180 CST Singapore vs 3.5% Fuel FOB Rotterdam Barges", "HSFO 180 CST Singapore (6.5) vs 3.5% Fuel FOB Rotterdam Barges")
    )

    starling.inTransaction{
      writer => {
        names.map {
          case (oldName, newName) => {
            writer.update("update markets set name = :newName where name = :oldName", Map("newName" -> newName, "oldName" -> oldName))
          }
        }
      }
    }
    MarketProvider.reload
  }
}