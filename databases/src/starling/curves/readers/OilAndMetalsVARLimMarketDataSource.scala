package starling.curves.readers

import starling.LIMServer
import starling.market._
import collection.SortedMap
import starling.marketdata._
import starling.utils.ImplicitConversions._
import collection.immutable.Map
import starling.daterange._
import java.lang.String
import starling.pivot.MarketValue
import starling.concurrent.MP._
import starling.db.{NoMarketDataForDayException, MarketDataEntry, MarketDataSource}
import starling.curves.MissingMarketDataException
import starling.utils.Log


class OilAndMetalsVARLimMarketDataSource(limServer: LIMServer) extends MarketDataSource {
  val daysInThePast = 365

  def read(day:Day) = {
    val (futuresFrontPeriodIndexes, publishedIndexes) = {
      val futuresFrontPeriodIndexes = (Index.futuresMarketIndexes).filter(index => index.market.limSymbol.isDefined)
      val publishedIndexes = Index.publishedIndexes.filter(_.market.limSymbol.isDefined)

      val ambiguous = (futuresFrontPeriodIndexes.map(_.market.asInstanceOf[CommodityMarket]) & publishedIndexes.map(_.market)).toList
      //ambiguous.require(_.isEmpty, "Ambiguous markets (both published & futures front period):")

      (futuresFrontPeriodIndexes.filterNot(index => ambiguous.contains(index.market)),
                publishedIndexes.filterNot(index => ambiguous.contains(index.market)))
    }

    val fixingsForPublishedIndex = {
      val pubMarkets = publishedIndexes.map(index => (index.market, index.level)).groupInto(_._1, _._2)

      pubMarkets.flatMap { case (market, levels) => {
        if (market.name == "Gas Oil 0.2 CIF NWE Cargoes") {
          println("odd market")
        }
        val pricesByObservationDay: Map[Day, Traversable[(Level, Double)]] = levels.flatMap { level => {
          val spotData: Map[Day, Double] = try {
            limServer.getSpotData(market.limSymbol.get, level, day - daysInThePast, day)
          } catch {
            case m: MissingMarketDataException => {
              Log.warn("No market data for " + market.limSymbol)
              Map()
            }
          }
          spotData.map {
            case (d: Day, p: Double) => (d, (level, p * market.limSymbol.get.multiplier))
          }
        } }.groupInto(_._1, _._2)

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

    Map((day - daysInThePast, day, PriceFixingsHistoryDataType) → fixings)
  }


  private def readFuturesFixingsFromLim(lastObservationDay: Day, market: FuturesMarket, level: Level): List[MarketDataEntry] = {
    val limSymbol = market.limSymbol.getOrElse(throw new Exception("No Lim symbol for market: " + market))

    def parseRelation(relation: String) = {
      val monthPart = relation.substring(limSymbol.name.size + 1)

      ReutersDeliveryMonthCodes.parse(monthPart).map(month => relation → month.asInstanceOf[DateRange])
    }
    val pricesByObservationPoint = limServer.query {
      connection =>
        val relationToDeliveryMonth = connection.getAllRelChildren(limSymbol.name).flatMapO(parseRelation).toMap

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
    }.toList.sortBy(_.observationPoint.day)
    allFixings
  }
}