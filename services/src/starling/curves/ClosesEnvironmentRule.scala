package starling.curves

import starling.daterange._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.db.MarketDataReader
import starling.quantity.UOM
import starling.market._

import ObservationTimeOfDay._
import starling.market.FuturesExchangeFactory._
import starling.gui.api.{PricingGroup, EnvironmentRuleLabel}

object ClosesEnvironmentRule {
  def label(allowOldPricesToBeUsed: Boolean = false): EnvironmentRuleLabel =
    if (allowOldPricesToBeUsed) EnvironmentRuleLabel("Most recent closes") else EnvironmentRuleLabel.AllCloses
}

case class ClosesEnvironmentRule(referenceDataLookup: ReferenceDataLookup, allowOldPricesToBeUsed : Boolean = false) extends EnvironmentRule {
  val pricingGroups = List(PricingGroup.Metals)
  val exchangeCloses : Map[FuturesExchange, ObservationTimeOfDay] = Map(SFS → SHFEClose, LME → LMEClose, COMEX → COMEXClose)
  val marketCloses = exchangeCloses.composeKeys((market: FuturesMarket) => market.exchange)

  val label = ClosesEnvironmentRule.label(allowOldPricesToBeUsed)
  private val numberOfDaysToLookBack = if (allowOldPricesToBeUsed) 7 else 0
  
  lazy val marketsWithCloseTimeOfDay = Market.futuresMarkets.optPair(marketCloses.get(_)).toList

  override def createNullAtomicEnvironment(observationDay: Day) = new NullAtomicEnvironment(observationDay.endOfDay, referenceDataLookup)

  def createEnv(observationDay: Day, marketDataReader: MarketDataReader): EnvironmentWithDomain = {

    def read_(timeOfDay : ObservationTimeOfDay, key : MarketDataKey) : MarketData = {
      marketDataReader.readMostRecent(numberOfDaysToLookBack, observationDay, timeOfDay, key)
    }
    val priceDataMap = marketsWithCloseTimeOfDay.flatMap {
      case (market, timeOfDay) => try {
        val marketData = read_(timeOfDay, PriceDataKey(market))
        Some(PriceDataKey(market) → marketData.asInstanceOf[PriceData])
      } catch {
        case e: MissingMarketDataException => None
      }
    }.toMap

    val reader = new MarketDataSlice {
      def read(key: MarketDataKey) = read(key, observationDay)

      private def read(key: MarketDataKey, obsDay : Day) : MarketData = {
        try {
          key match {
            case priceDataKey@PriceDataKey(market) => {
              priceDataMap.getOrElse(priceDataKey, throw new Exception("No prices for " + market))
            }
            case key: ForwardRateDataKey => read_(ObservationTimeOfDay.Default, key)
            case key: CountryBenchmarkMarketDataKey => read_(ObservationTimeOfDay.Default, key)
            case key: GradeAreaBenchmarkMarketDataKey => read_(ObservationTimeOfDay.Default, key)
            case key: FreightParityDataKey => read_(ObservationTimeOfDay.Default, key)
            case key@SpotFXDataKey(UOM.CNY) => read_(ObservationTimeOfDay.SHFEClose, key)
            case key: SpotFXDataKey => read_(ObservationTimeOfDay.LondonClose, key)
            case _ => throw new Exception(name + " Closes Rule has no rule for " + key)
          }
        } catch {
          case _: MissingMarketDataException if allowOldPricesToBeUsed && (observationDay - obsDay.previousWeekday <= numberOfDaysToLookBack) => read(key, obsDay.previousWeekday)
          case e => throw e
        }
      }

      def fixings(key : PriceFixingsHistoryDataKey, observationPoint: ObservationPoint): PriceFixingsHistoryData = {
        val closeTime = FuturesExchangeFactory.fromName(key.exchangeName).map(_.closeTime)

        key.read(observationPoint.copyTime(closeTime), marketDataReader)
      }
    }

    val marketsX = {
      priceDataMap.toList.map { case (PriceDataKey(futuresMarket: FuturesMarket), priceData) => {
        UnderlyingDeliveryPeriods(marketCloses(futuresMarket), futuresMarket, priceData.sortedKeys)
      }
      case (PriceDataKey(market: PublishedIndex), priceData) => {
        UnderlyingDeliveryPeriods(ObservationTimeOfDay.LondonClose, market, priceData.sortedKeys)
      } }
    }

    val environmentX = Environment(new MarketDataCurveObjectEnvironment(if (allowOldPricesToBeUsed) observationDay.startOfDay else observationDay.endOfDay, reader, false, referenceDataLookup))

    new EnvironmentWithDomain {
      val environment = environmentX
      def markets = marketsX
      override def discounts = marketDataReader.readAll(ForwardRateDataType.name, observationDay.atTimeOfDay(ObservationTimeOfDay.Default)).map {
        case (key:ForwardRateDataKey, data:ForwardRateData) => key.ccy -> data
      }

      override def spotFX = marketDataReader.readAll(SpotFXDataType.name, observationDay.atTimeOfDay(ObservationTimeOfDay.LondonClose)).map {
        case (key:SpotFXDataKey, _) => key.ccy
      }
    }
  }
}
