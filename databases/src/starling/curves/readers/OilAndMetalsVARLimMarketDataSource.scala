package starling.curves.readers

import starling.market._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import collection.immutable.Map
import starling.daterange._
import java.lang.String
import starling.pivot.MarketValue
import starling.lim.LIMService
import starling.db.{MarketDataSet, MarketDataEntry, MarketDataSource}
import starling.quantity.UOM._
import starling.db.{NoMarketDataForDayException, MarketDataEntry, MarketDataSource}
import starling.curves.MissingMarketDataException
import starling.quantity.Percentage
import starling.utils._

class OilAndMetalsVARLimMarketDataSource(service: LIMService, override val marketDataSet: MarketDataSet) extends MarketDataSource {
  val daysInThePast = 365

  def read(day:Day) = {
    val (futuresFrontPeriodIndexes, publishedIndexes) = {
      val futuresFrontPeriodIndexes = (Index.futuresMarketIndexesView).filter(index => index.market.limSymbol.isDefined)
      val publishedIndexes = Index.publishedIndexesView.filter(_.market.limSymbol.isDefined)

      val ambiguous = (futuresFrontPeriodIndexes.map(_.market.asInstanceOf[CommodityMarket]) & publishedIndexes.map(_.market)).toList
      //ambiguous.require(_.isEmpty, "Ambiguous markets (both published & futures front period):")

      (futuresFrontPeriodIndexes.filterNot(index => ambiguous.contains(index.market)),
                publishedIndexes.filterNot(index => ambiguous.contains(index.market)))
    }

    val fixingsForPublishedIndex = {
      val pubMarkets = publishedIndexes.map(index => (index.market, index.level)).toMultiMap

      pubMarkets.flatMap { case (market, levels) => {
        val pricesByObservationDay: Map[Day, Traversable[(Level, Double)]] = levels.flatMap { level => {
          val spotData: Map[Day, Double] = try {
            service.getSpotData(market.limSymbol.get, level, day - daysInThePast, day)
          } catch {
            case m: MissingMarketDataException => {
              Log.warn("No market data for " + market.limSymbol)
              Map()
            }
          }
          spotData.map {
            case (d: Day, p: Double) => (d, (level, MathUtil.roundToNdp(p * market.limSymbol.get.multiplier, 6)))
          }
        } }.toMultiMap

        pricesByObservationDay.map {
          case (day, prices) => {
            val fixingsHistoryData = PriceFixingsHistoryData.create(prices.map {
              case (level, price) => (level, StoredFixingPeriod.dateRange(day)) → (market.priceUOM match {
                case SCALAR => {
                  val percentage = MarketValue.percentage(Percentage.fromPercentage(price))
                  percentage
                }
                case _ => MarketValue.quantity(price, market.priceUOM)
              })
            }.toMap)
            MarketDataEntry(ObservationPoint(day), PriceFixingsHistoryDataKey(market), fixingsHistoryData)
          }
        }
      } }
    }.toList

    val fixingsForFrontMonthIndexes = futuresFrontPeriodIndexes.mapDistinct(_.market)
      .flatMap(market => readFuturesFixingsFromLim(day, market, Level.Close)).toList

    val fixings = fixingsForPublishedIndex ::: fixingsForFrontMonthIndexes

    Map((day - daysInThePast, day, PriceFixingsHistoryDataType.name) → fixings)
  }


  private def readFuturesFixingsFromLim(lastObservationDay: Day, market: FuturesMarket, level: Level): List[MarketDataEntry] = {
    val limSymbol = market.limSymbol.getOrElse(throw new Exception("No Lim symbol for market: " + market))

    def parseRelation(relation: String): Option[Relation] = {
      val monthPart = relation.substring(limSymbol.name.size + 1)

      ReutersDeliveryMonthCodes.parse(monthPart).map(Relation(relation, _))
    }

    val pricesByObservationPoint = service.query { connection => {
      val relations = connection.getAllRelationChildren(limSymbol.name).flatMapO(parseRelation).toList
      val prices = connection.getPrices(relations, level, lastObservationDay - daysInThePast, lastObservationDay)

      val roundedPrices = prices.mapNested { case (observationDay, relation, price) => {
        val roundedPrice = MathUtil.roundToNdp(price * limSymbol.multiplier, 6)

        (ObservationPoint(observationDay), (level, relation.period) → MarketValue.quantity(roundedPrice, market.priceUOM))
      } }.toNestedMap

      roundedPrices
    } }

    val allFixings = pricesByObservationPoint.map { case (observationPoint, fixings) =>
      MarketDataEntry(observationPoint, PriceFixingsHistoryDataKey(market), PriceFixingsHistoryData.create(fixings))
    }.toList.sortBy(_.observationPoint.day)

    allFixings
  }

  private case class Relation(relation: String, deliveryMonth: DateRange) {
    val period = StoredFixingPeriod.dateRange(deliveryMonth)
    override def toString = relation
  }
}