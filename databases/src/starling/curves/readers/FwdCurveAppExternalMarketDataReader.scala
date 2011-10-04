package starling.curves.readers


import starling.utils.Log
import starling.daterange._
import starling.db._
import starling.curves._
import starling.marketdata._
import starling.quantity.UOM._
import starling.utils.ImplicitConversions._
import starling.quantity.{Percentage, UOM, Quantity}
import starling.utils.cache.CacheFactory
import starling.props.Props
import starling.market._
import starling.LIMServer
import collection.SortedMap
import starling.gui.api.MarketDataIdentifier
import collection.immutable.{Iterable, TreeMap}


object FwdCurveAppExternalMarketDataReader {
  // TODO [25 Mar 2010] refactor somewhere sensible
  val currencyCurveIDs = Map(JPY -> 10010, MYR -> 10020, GBP -> 10011, CAD -> 10013, EUR -> 10014, CNY -> 10015)
  val forwardRateCurveIDs = Map(USD -> 1911)
  // note: is a map of market to a Map of spread size to List(atm, call, put) of curve IDs
  val spreadStandardDeviationCurveIDs : Map[FuturesMarket, Map[Int, List[Int]]] = Map(
    Market.NYMEX_WTI -> Map(
      1 -> List(2471, 2472, 2473),
      2 -> List(3134, 3135, 3136),
      6 -> List(3137, 3138, 3139),
      12 -> List(3140, 3141, 3142)))

  def constructSpreadStdDev(
          market : FuturesMarket,
          uom : UOM,
          priceLookup : (List[Int]) => Option[List[PriceData]]
          ) : Option[SpreadStdDevSurfaceData] = {
    val builder = new SpreadStdDevSurfaceDataBuilder(Some(uom))
    spreadStandardDeviationCurveIDs.get(market).foreach(curveInfo => {
      curveInfo.foreach (tuple => {
        val (spreadSize, curveIDs) = tuple
        priceLookup(curveIDs).foreach(curveData => {
          val atmCurve :: callCurve :: putCurve :: Nil = curveData
          if (atmCurve.prices.nonEmpty &&
              (atmCurve.prices.keySet == callCurve.prices.keySet) &&
              (atmCurve.prices.keySet == putCurve.prices.keySet)) {
            val months : Array[Month] = atmCurve.prices.keySet.filter(_.tenor == Some(Month)).map(_.asInstanceOf[Month]).toArray.sortWith(_ < _)

            months.zipWithIndex.foreach { case(m, index) => {
              val period = SpreadPeriod(m, m + spreadSize)
              builder.addAtm(period, atmCurve.prices(m).quantityValue.get)
              builder.addCall(period, callCurve.prices(m).quantityValue.get)
              builder.addPut(period, putCurve.prices(m).quantityValue.get)
            } }
          }
        })
      })
    })
    builder.buildIfDefined
  }
}