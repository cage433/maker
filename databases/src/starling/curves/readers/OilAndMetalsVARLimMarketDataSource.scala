package starling.curves.readers

import starling.LIMServer
import starling.market._
import collection.SortedMap
import starling.marketdata._
import starling.db.{MarketDataEntry, MarketDataSource}
import starling.utils.ImplicitConversions._
import collection.immutable.Map
import starling.daterange._
import java.lang.String
import starling.pivot.MarketValue
import starling.concurrent.MP._


class OilAndMetalsVARLimMarketDataSource(limServer: LIMServer) extends MarketDataSource {
  val daysInThePast = 100

  def read(day:Day) = {
    val (futuresFrontPeriodIndexes, publishedIndexes) = {
      val futuresFrontPeriodIndexes =
        (FuturesFrontPeriodIndex.knownFrontFuturesIndices ::: FuturesFrontPeriodIndex.unknownFrontFuturesIndices).filter(index =>
          index.market.limSymbol.isDefined)

      val publishedIndexes = PublishedIndex.publishedIndexes.filter(_.market.limSymbol.isDefined)

      val ambiguous = (futuresFrontPeriodIndexes.map(_.market.asInstanceOf[CommodityMarket]) & publishedIndexes.map(_.market)).toList
      //ambiguous.require(_.isEmpty, "Ambiguous markets (both published & futures front period):")

      (futuresFrontPeriodIndexes.filterNot(index => ambiguous.contains(index.market)),
                publishedIndexes.filterNot(index => ambiguous.contains(index.market)))
    }

//    val indexes = Index.indicesToImportFixingsForFromEAI
//    val trinityIndexes = TrinityIndex.trinityIndexes

//    futuresFrontPeriodIndexes.foreach(index => println("ffp: " + (index, index.market, index.limSymbol)))
//    publishedIndexes.foreach(index => println("pi: " + (index, index.market, index.limSymbol)))

  //  println(FuturesFrontPeriodIndex.WTI10.market.lastTradingDay(Month(2010, 11)))

//    futuresFrontPeriodIndexes.foreach{ index => {
//      println( index )
//      try {
//        val limFixings = readFuturesFixingsFromLim(day, index).map{ mde =>
//          mde.observationPoint.day → mde.data.asInstanceOf[PriceFixingsHistoryData].fixings.values.iterator.next }
//        val marketBasedFixings = readFuturesFixingsFromLim(day, index.market, index.level).map { mde =>
//          val history = mde.data.asInstanceOf[PriceFixingsHistoryData]
//          mde.observationPoint.day → history.fixingFor(index.level, index.observedPeriod(mde.observationPoint.day))
//        }
//
//        (limFixings.sortBy(_._1) zip marketBasedFixings.sortBy(_._1)).foreach { case (lim,mkt) => {
//          if (lim != mkt) println( (lim, mkt) )
//        } }
//
//        println
//      } catch { case e: Exception => {e.printStackTrace}
//      }
//    } }
//

    val fixingsForPublishedIndex = {
      val pubMarkets = publishedIndexes.map(index => (index.market, index.level)).groupInto(_._1, _._2)

      pubMarkets.flatMap { case (market, levels) => {
        val pricesByObservationDay = levels.flatMap { level =>
          limServer.getSpotData(market.limSymbol.get, level, day - daysInThePast, day).map {
            case (d: Day, p: Double) => (d, (level, p * market.limSymbol.get.multiplier))
          }
        }.groupInto(_._1, _._2)

        pricesByObservationDay.map { case (day, prices) =>
          MarketDataEntry(ObservationPoint(day), PriceFixingsHistoryDataKey(market),
            PriceFixingsHistoryData.create(prices.map { case (level, price) =>
              (level, StoredFixingPeriod.dateRange(day)) → MarketValue.quantity(price, market.priceUOM)
            }.toMap)
          )
        }
      } }
    }.toList

    val fixingsForFrontMonthIndexes = futuresFrontPeriodIndexes.mapDistinct(_.market)
      .flatMap(market => readFuturesFixingsFromLim(day, market, Level.Close)).toList

    val fixings = fixingsForPublishedIndex ::: fixingsForFrontMonthIndexes

    val prices: List[MarketDataEntry] = Market.futuresMarkets.flatMap {
      market => {
        market match {
          case mkt : Market.BalticFuturesMarket => Some(PriceDataKey(mkt) -> readFreightPricesFromLim(day, mkt))
          case _ => None
        }
      }
    }.map{ case(key, data) => MarketDataEntry(ObservationPoint(day), key, data) }

    Map((day - daysInThePast, day, PriceFixingsHistoryDataType) → fixings,
        (day, day, PriceDataType) → prices)
  }

  private def readFreightPricesFromLim(day:Day, mkt : Market.BalticFuturesMarket) : PriceData = {
    val rawLimPriceData : Array[Double] = limServer.getMultipleData(mkt.limSymbols.map(_.name), day, day)(day)
    val rawPriceMap = mkt.limSymbols.map{ls => mkt.periodForLimSymbol(day.endOfDay, ls)}.zip(rawLimPriceData).filter{
      case (_, price) => ! price.isNaN && ! price.isInfinity && price > 0
    }.toMap
    PriceData.create(SortedMap[DateRange, Double]() ++ rawPriceMap, mkt.priceUOM)
  }

  private def readFuturesFixingsFromLim(lastObservationDay: Day, market: FuturesMarket, level: Level): List[MarketDataEntry] = {
    val limSymbol = market.limSymbol.getOrElse(throw new Exception("No Lim symbol for market: " + market))

    def parseRelation(relation: String) = {
      val monthPart = relation.substring(limSymbol.name.size + 1)

      ReutersDeliveryMonthCodes.parse(monthPart).map(month => relation → month.asInstanceOf[DateRange])
    }
    val pricesByObservationPoint = limServer.query {
      connection =>
        val relationToDeliveryMonth = connection.getAllRelChildren(limSymbol.name).map(parseRelation).somes.toMap

        val prices = connection.getPrices(relationToDeliveryMonth.keys, level, lastObservationDay - daysInThePast, lastObservationDay)
          .groupInto {
          case ((relation, observationDay), price) => {
            (ObservationPoint(observationDay), relationToDeliveryMonth(relation) → MarketValue.quantity(price * limSymbol.multiplier, market.priceUOM))
          }
        }
        prices
    }

    val allFixings = pricesByObservationPoint.map {
      case (observationPoint, deliveryMonthToPrice) =>
        val fixings = deliveryMonthToPrice.toMap.mapKeys(deliveryMonth => (level, StoredFixingPeriod.dateRange(deliveryMonth)))

        MarketDataEntry(observationPoint, PriceFixingsHistoryDataKey(market), PriceFixingsHistoryData.create(fixings))
    }.toList
    allFixings
  }
}