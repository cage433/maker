package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.tradestore.eai.EAITradeStore

class Patch130_ChangeMarketFormulasToIncludeQuotePart extends Patch {

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    starling.inTransaction {
      writer => {
        writer.queryForUpdate("select formula from Markets where formula is not null") {
          q => {
            val f = q.getString("formula")
            var formula = ""
            var from = 0
            do {
              val index = f.indexWhere(c => c == '+' || c == '-', from + 1)
              if (index <= 0) {
                formula += "Quote(" + f.substring(from).trim + ") "
                from = index
              } else {
                val q = f.substring(from, index).trim
                val op = f.charAt(index)
                formula += "Quote(" + q + ") " + op + " "
                from = index + 1
              }
            } while (from > 0)
            q.update(Map("formula" -> formula))
          }
        }
      }
    }
  }

}
