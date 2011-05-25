package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.marketdata.OilVolSurfaceDataKey
import starling.utils.sql.AnObject
import starling.curves.SpreadStdDevSurfaceDataKey
import starling.market.{FuturesMarket, Market}
import starling.services.StarlingInit

class Patch49_RenameOilMarkets extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val names = (Market.markets.map(m => m.name -> m).toMap ++
            Map("Fuel Oil 3.5 FOB" -> Market.FUEL_FOB_ROTTERDAM_BARGES_3_5, "HSFO 180 CST FOB" -> Market.HSFO_180_CST_Singapore,
              "USGC Waterborne 6.3" -> Market.No_6_3PC_USGC_Waterborne, "Nymex RBOB" -> Market.NYMEX_GASOLINE)).map(e => e._1.toLowerCase -> e._2)

    val MarketNameOnly = """(?s).*<market>(.+)</market>.*""".r
    val MarketNameInClass = """(?s).*<market class="starling\.market\.(.+)">(.+)</market>.*""".r

    starling.inTransaction {
      writer => {
        {
          writer.queryForUpdate("select marketDataKey from MarketData where marketDataType = '<starling.marketdata.OilVolSurfaceDataType_-/>'") {
            rs => {
              rs.getString("marketDataKey") match {
                case MarketNameInClass(_, name) => {
                  val market = names(name.toLowerCase)
                  val key = new OilVolSurfaceDataKey(market)
                  rs.update(Map("marketDataKey" -> AnObject(key)))
                }
                case MarketNameOnly(name) => {
                  val market = names(name.toLowerCase)
                  val key = new OilVolSurfaceDataKey(market)
                  rs.update(Map("marketDataKey" -> AnObject(key)))
                }
              }
            }
          }
        }
        {
          writer.queryForUpdate("select marketDataKey from MarketData where marketDataType = '<starling.curves.SpreadStdDevSurfaceDataType_-/>'") {
            rs => {
              rs.getString("marketDataKey") match {
                case MarketNameOnly(name) => {
                  val market = FuturesMarket.fromName(name)
                  val key = new SpreadStdDevSurfaceDataKey(market)
                  rs.update(Map("marketDataKey" -> AnObject(key)))
                }
                case MarketNameInClass(_, name) => {
                  val market = FuturesMarket.fromName(name)
                  val key = new SpreadStdDevSurfaceDataKey(market)
                  rs.update(Map("marketDataKey" -> AnObject(key)))
                }
              }
            }
          }
        }
      }
    }

  }

  def patchDescription = "Rename oil vol markets after removing OilVolMarket class"
}