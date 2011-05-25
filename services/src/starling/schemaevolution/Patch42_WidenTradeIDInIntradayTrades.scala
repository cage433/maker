package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch42_WidenTradeIDInIntradayTrades  extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    starling.inTransaction {
      newWriter => {
        writer.update("alter table IntradayTrades alter column tradeID varchar(255) not null")
      }
    }
  }

  def patchDescription = "make tradeid colum longer now that tradeid includes the excel label"
}