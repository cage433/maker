package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.{PatchUtils, Patch}
import starling.utils.sql.PersistAsBlob
import starling.marketdata.PriceDataKey
import starling.market.{FuturesMarket, Market}
import starling.services.StarlingInit

class Patch46_ConvertMarketDataKeysWithBadMarketNames extends Patch {

  // <starling.marketdata.BradyFreightVolsDataKey>   <market class="starling.market.Market$BalticFuturesMarket">Baltic Capesize TC Avg</market>   <dataType/>   <subTypeKey>Baltic Capesize TC Avg</subTypeKey> </starling.marketdata.BradyFreightVolsDataKey>
  //<starling.marketdata.PriceDataKey>   <market class="starling.market.Market$$anon$20">LBMA Gold</market> </starling.marketdata.PriceDataKey>
  val MarketName = """(?s).*<market class="starling\.market\.(.+)">(.+)</market>.*""".r

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {

     // Redundant since we are removing Brady market data types
    throw new Exception("Patch46_ConvertMarketDataKeysWithBadMarketNames should no longer be run")
//    starling.inTransaction {
//      writer => {
//        writer.queryForUpdate("select marketDataType, marketdatakey from MarketData where marketdatakey like '%market class=\"starling.market.%Market%' and marketDataType != '<starling.marketdata.OilVolSurfaceDataType_-/>'") {
//          rs => {
//            val mdk = rs.getString("marketdatakey")
//            val MarketName(_, name) = mdk
//            val market = Market.fromName(name)
//            rs.getString("marketDataType") match {
//              case "<starling.marketdata.BradyMetalVolsDataType_-/>" => {
//                rs.update(Map("marketdatakey" -> AnObject(BradyMetalVolsDataKey(market))))
//              }
//              case "<starling.marketdata.PriceDataType_-/>" => {
//                rs.update(Map("marketdatakey" -> AnObject(PriceDataKey(market))))
//              }
//              case "<starling.marketdata.BradyFreightVolsDataType_-/>" => {
//                rs.update(Map("marketdatakey" -> AnObject(BradyFreightVolsDataKey(market.asInstanceOf[FuturesMarket]))))
//              }
//            }
//          }
//        }
//      }
//    }
    starling.inTransaction {
      writer => {
        writer.queryForUpdate("select marketdatakey from MarketData where marketDataType != '<starling.marketdata.OilVolSurfaceDataType_-/>'") {
          rs => {
            val obj: Object = rs.getObject("marketdatakey")
            rs.update(Map("marketdatakey" -> PersistAsBlob(obj)))
          }
        }
      }
    }
  }

  def patchDescription = "Convert market data keys that have bad market names"
}
