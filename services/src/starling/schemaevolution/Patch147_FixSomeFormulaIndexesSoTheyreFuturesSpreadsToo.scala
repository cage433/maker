package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchUtils, Patch}
import starling.daterange.Month


class Patch147_FixSomeFormulaIndexesSoTheyreFuturesSpreadsToo extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val updates = List(("NYMEX Heat vs NYMEX WTI", 1000),
      ("NYMEX Unleaded vs NYMEX WTI", 1000),
      ("NYMEX Unleaded vs NYMEX Heat", 42000),
      ("NYMEX Heat vs ICE Gas Oil 1st month", 42000))

    updates.map{
      case (name, lotsize) => {
        writer.update("update markets set lotsize = :lotsize, tenor = :tenor where name = :name",
          Map("lotsize" -> Option(Option(lotsize)), "name" -> name, "tenor" -> "Month"))
      }
    }
  }

  override def requiresRestart = true
}