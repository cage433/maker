package starling.schemaevolution

import system.Patch
import starling.richdb.RichDB
import starling.services.StarlingInit
import starling.marketdata.PriceFixingsHistoryDataType
import starling.utils.sql.PersistAsBlob
import starling.daterange.Day
import starling.db.{MarketDataSet, DBWriter}
import starling.utils.sql.QueryBuilder._

class Patch106_AddSpreadMarkets extends Patch {
  val RB_CRACKS = ("RB Cracks", "Nymex RBOB vs Nymex WTI", 1000.0)
  val RB_BRENT_CRACKS = ("RB Brent Cracks", "NYMEX RBOB 1st Month vs IPE Brent 1st Month", 1000.0)
  val RBHO = ("RBHO", "Nymex RBOB vs Nymex Heat", 42000.0)
  val GO_CRACKS = ("GO Cracks", "IPE Gas Oil (Settlement) vs IPE Brent", 1000.0)
  val ICE_WTI_BRENT = ("WTI Brent", "ICE WTI 1st month vs ICE Brent 1st month", 1000.0)
  val NYMEX_WTI_BRENT = ("NY WTI Brent", "NYMEX WTI vs IPE Brent", 1000.0)
  val names = List(RB_CRACKS, RB_BRENT_CRACKS, RBHO, GO_CRACKS, ICE_WTI_BRENT, NYMEX_WTI_BRENT)

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    names.map {
      case (oldName, newName, lotSize) => {
        writer.update("intradayTrades", Map("market" -> newName), ("market" eql oldName))
        writer.update("markets", Map("type" -> "FormulaIndex/FuturesSpreadMarket", "lotSize" -> Some(lotSize).toString, "tenor" -> "Month"), ("name" eql newName))

        if(newName == ICE_WTI_BRENT._2) {
          writer.update("markets", Map("expiryRule" -> "Some(WTI-Brent Crude Oil Spread Expiry)"), ("name" eql newName))
        }
      }
    }

    writer.queryForUpdate("select * from marketdata where marketDAtaType = '<starling.curves.SpreadStdDevSurfaceDataType_-/>'") {
      rs => {
        val old = rs.getString("data")
        val newS = old
          .replaceAll("<starling.daterange.Spread>", "<starling.daterange.SpreadPeriod>")
          .replaceAll("""<bitmap_\-0>0</bitmap_\-0>""", "")
          .replaceAll("</starling.daterange.Spread>", "</starling.daterange.SpreadPeriod>")
          .replaceAll("first class", "front class")
          .replaceAll("/first", "/front")
          .replaceAll("/last", "/back")
        rs.update(Map("data" -> newS))
      }
    }
  }

  def patchDescription = "Add spread markets"
}
