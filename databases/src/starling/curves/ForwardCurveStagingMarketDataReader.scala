package starling.curves

import starling.curves.readers.FwdCurveAppExternalMarketDataReader
import starling.marketdata._
import starling.richdb.RichDB
import starling.utils.sql.QueryBuilder
import starling.db._
import starling.LIMServer
import starling.quantity.{Percentage, UOM}
import collection.SortedMap
import collection.immutable.{TreeMap, TreeSet}
import starling.daterange.{DateRange, Spread, Month, Day}

case class ForwardCurveStagingPricingGroup(
        pricingGroupID : Int,
        maybeName : Option[String]
        ) {
  override def toString = pricingGroupID.toString + maybeName.map("/" + _).getOrElse("")
  def useForwardCurveAppPrices = false
}

// TODO [08 Jun 2010] remove as soon as you have a better idea
//trait ForwardCurveStagingInterface {
//  def apply(curveID:Int, observationDate:Day, pricingGroupID:Int, subgroupName:Option[String]) : Option[PriceData]
//}
//
//class ForwardCurveStagingMarketDataSource(
//        fcspg : ForwardCurveStagingPricingGroup,
//        fcStagingDB : ForwardCurveStagingDB,
//        fcDB : RichDB,
//        limServer : LIMServer,
//        eaiDB : RichDB,
//        fcInterface : ForwardCurveStagingInterface
//        ) extends MarketDataSource {
//
//  def read(observationDay: Day) = {
//    // TODO [07 Sep 2010] this is a copy of the forward curve data - should be rewritten to take into account
//    // what the user has acutally provided.
//    val reader = new ForwardCurveStagingMarketDataReader(fcspg, observationDay, fcStagingDB, fcDB, limServer, eaiDB, fcInterface)
//    val spotFXKeys:List[MarketDataKey] = FwdCurveAppExternalMarketDataReader.currencyCurveIDs.keysIterator.map{ccy=>SpotFXDataKey(ccy)}.toList
//    val priceKeys:List[MarketDataKey] = PriceDataType.keys
//    val indexKeys:List[MarketDataKey] = PriceFixingsHistoryDataType.keys
//    val forwardRateKeys:List[MarketDataKey] = List(ForwardRateDataKey(UOM.USD))
//    val volSurfaceKeys:List[MarketDataKey] = OilVolSurfaceDataType.keys
//    val spreadStdDevKeys:List[MarketDataKey] = SpreadStdDevSurfaceDataType.keys
//    val allKeys = spotFXKeys ::: priceKeys ::: indexKeys ::: forwardRateKeys ::: volSurfaceKeys ::: spreadStdDevKeys
//    Map() ++ allKeys.flatMap {
//      key => try {
//        Some((key, key.read(reader)))
//      } catch {
//        case e:MissingMarketDataException => None
//      }
//    }
//  }
//
//  def localPricingGroups = fcStagingDB.distinctIdentifiers.map((tuple) => {
//    val (pgID, maybeName) = tuple
//    ForwardCurveStagingPricingGroup(pgID, maybeName)
//  })
//}
