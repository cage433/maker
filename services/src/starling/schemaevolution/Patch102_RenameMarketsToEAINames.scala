package starling.schemaevolution

import system.Patch
import starling.richdb.RichDB
import starling.services.StarlingInit
import starling.marketdata.PriceFixingsHistoryDataType
import starling.utils.sql.AnObject
import starling.daterange.Day
import starling.db.{MarketDataSet, DBWriter}

class Patch102_RenameMarketsToEAINames extends Patch {
  val names = Map("Baltic Capesize C7 Bolivar to Rotterdam" -> "Capesize C7 Bolivar to Rotterdam (Baltic)",
    "Baltic Capesize TC Avg" -> "Capesize T/C Average (Baltic)",
    "Baltic Panamax TC Avg" -> "Panamax T/C Average diff (Baltic)",
    "Baltic Supramax TC Avg" -> "Baltic Supramax T/C Avg",
    "Baltic Handymax TC Avg" -> "Handymax T/C Average (Baltic)",
    "Gas Oil ULSD 10ppm CIF NWE Cargoes" -> "Gas Oil ULSD 10ppm CIF NWE Cargoes (Platts)",
    "Gas Oil ULSD 10ppm FOB Rotterdam Barges" -> "Gas Oil ULSD 10ppm FOB Rotterdam Barges (Platts)",
    "ICE Brent" -> "IPE Brent",
    "ICE Gas Oil" -> "IPE Gas Oil",
    "Iron Ore" -> "Iron Ore CFR China 62% Fe",
    "NYMEX Nat Gas" -> "NYMEX Henry Hub Natural Gas",
    "RBOB" -> "NYMEX RBOB",
    "Steel Rebar Shanghai" -> "SHFE Steel Rebar",
    "Panamax T/C Avg" -> "Panamax T/C Average diff (Baltic)",
    "Supramax T/C Avg" -> "Baltic Supramax T/C Avg",
    "Capsize T/C Avg" -> "Capesize T/C Average (Baltic)",
    "C7 T/C Avg" -> "Capesize C7 Bolivar to Rotterdam (Baltic)",
    "Gas Oil ULSD 10ppm CIF NWE Cargoes" -> "Gas Oil ULSD 10ppm CIF NWE Cargoes (Platts)",
    "Gas Oil ULSD 10ppm FOB Rotterdam Barges" -> "Gas Oil ULSD 10ppm FOB Rotterdam Barges (Platts)",
    "NYMEX Heat front month price" -> "NYMEX Heat 1st month",
    "Platts Dubai front month price" -> "Platts Dubai 1st month",
    "ICE WTI front month price" -> "ICE WTI 1st month",
    "ICE Brent front month price" -> "IPE Brent 1st month",
    "Platts Brent front month price" -> "Platts Brent 1st Month",
    "RBOB front month price" -> "NYMEX RBOB 1st month",
    "NYMEX WTI front month price" -> "NYMEX WTI 1st month",
    "ICE Gas Oil front month price" -> "IPE Gas Oil 1st month (Settlement)",
    "ICE Gas Oil front month price" -> "NYMEX Gas Oil 1st month",
    "Gas Oil Crack" -> "IPE Gas Oil (Settlement) vs IPE Brent",
    "Unl 87 USGC Pipeline vs NYMEX WTIt" -> "Unl 87 USGC Pipeline vs NYMEX WTI",
    "Dated Brent vs Platts Brent JANUARY" -> "Dated Brent vs Platts Brent (January)",
    "Dated Brent vs Platts Brent FEBRUARY" -> "Dated Brent vs Platts Brent (February)",
    "Dated Brent vs Platts Brent MARCH" -> "Dated Brent vs Platts Brent (March)",
    "Dated Brent vs Platts Brent APRIL" -> "Dated Brent vs Platts Brent (April)",
    "Dated Brent vs Platts Brent MAY" -> "Dated Brent vs Platts Brent (May)",
    "Dated Brent vs Platts Brent JUNE" -> "Dated Brent vs Platts Brent (June)",
    "Dated Brent vs Platts Brent JULY" -> "Dated Brent vs Platts Brent (July)",
    "Dated Brent vs Platts Brent AUGUST" -> "Dated Brent vs Platts Brent (August)",
    "Dated Brent vs Platts Brent SEPTEMBER" -> "Dated Brent vs Platts Brent (September)",
    "Dated Brent vs Platts Brent OCTOBER" -> "Dated Brent vs Platts Brent (October)",
    "Dated Brent vs Platts Brent NOVEMBER" -> "Dated Brent vs Platts Brent (November)",
    "Dated Brent vs Platts Brent DECEMBER" -> "Dated Brent vs Platts Brent (December)"
  )

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    starling.inTransaction {
      writer: DBWriter => {
        {
          names.map {
            case (oldName, newName) => {
              writer.queryForUpdate("select marketDataKey from MarketData where marketDataKey like '%>" + oldName + "<%'") {
                rs => {
                  val key: String = rs.getString("marketDataKey")
                  val newKey = key.replace(oldName, newName)
                  rs.update(Map("marketDataKey" -> newKey))
                }
              }
            }
            val tables = List("EAITrade", "IntradayTrades")
            tables.map(table => writer.update("update " + table + " set market = :newMarket where market = :oldMarket", Map("newMarket" -> newName, "oldMarket" -> oldName)))
          }
        }
      }

      starling.inTransaction {
        writer => {
          writer.update("delete from MarketData where marketDataType = :t", Map("t" -> AnObject(PriceFixingsHistoryDataType)))
        }
      }

      starlingInit.marketDataStore.importFor(Day.today.previousWeekday, MarketDataSet("LIM"))
    }
  }

  def patchDescription = "Rename old starling markets to use EAI names"
}
