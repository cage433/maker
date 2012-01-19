package starling.curves

import starling.daterange._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.db.MarketDataReader
import starling.quantity.UOM
import starling.market._

import starling.gui.api.{PricingGroup, EnvironmentRuleLabel}

object AllClosesEnvironmentRule {
  def label(allowOldPricesToBeUsed: Boolean = false): EnvironmentRuleLabel =
    if (allowOldPricesToBeUsed) EnvironmentRuleLabel.MostRecentCloses else EnvironmentRuleLabel.AllCloses
}

abstract class ClosesEnvironmentRule extends EnvironmentRule{

  def marketDataSlice(marketDataReader : MarketDataReader, marketDataDay: Day): MarketDataSlice = {
    def readForThisMarketDataDay(timeOfDay: ObservationTimeOfDay, key: MarketDataKey): MarketData = {
      marketDataReader.read(TimedMarketDataKey(ObservationPoint(marketDataDay, timeOfDay), key))
    }
    val priceDataMap = Market.futuresMarketsView.safeMap {
      market => {
        market.exchange match {
          case FuturesExchangeFactory.COMEX | FuturesExchangeFactory.EXBXG => {
            val marketData = readForThisMarketDataDay(market.closeTime, PriceFixingsHistoryDataKey(market))
            PriceDataKey(market) → marketData.asInstanceOf[PriceFixingsHistoryData].toPriceData(Level.Close)
          }
          case _ => {
            val marketData = readForThisMarketDataDay(market.closeTime, PriceDataKey(market))
            PriceDataKey(market) → marketData.asInstanceOf[PriceData]
          }
        }
      }
    }.toMap

    new MarketDataSlice {
      def read(key: MarketDataKey): MarketData = {
        key match {
          case priceDataKey@PriceDataKey(market) => {
            priceDataMap.getOrElse(priceDataKey, throw new MissingMarketDataException(
              "No " + market + " prices",
              "No " + market + " prices on " + marketDataDay + "/" + market.asInstanceOf[FuturesMarket].closeTime.shortName
            ))
          }
          case key: ForwardRateDataKey => readForThisMarketDataDay(ObservationTimeOfDay.LiborClose, key)
          case key: CountryBenchmarkMarketDataKey => marketDataReader.read(TimedMarketDataKey(ObservationPoint.RealTime, key))
          case key: GradeAreaBenchmarkMarketDataKey => readForThisMarketDataDay(ObservationTimeOfDay.Default, key)
          case key: FreightParityDataKey => readForThisMarketDataDay(ObservationTimeOfDay.Default, key)
          case key: ShanghaiVATDataKey => marketDataReader.read(TimedMarketDataKey(ObservationPoint.RealTime, key))
          case key@SpotFXDataKey(UOM.CNY) => readForThisMarketDataDay(ObservationTimeOfDay.SHFEClose, key)
          case key: SpotFXDataKey => readForThisMarketDataDay(ObservationTimeOfDay.LondonClose, key)
          case _ => throw new Exception(name + " Closes Rule has no rule for " + key)
        }
      }

      def fixings(key: PriceFixingsHistoryDataKey, observationPoint: ObservationPoint): PriceFixingsHistoryData = {
        key.read(observationPoint, marketDataReader)
      }
    }
  }

}
case class UseMostRecentAtomicEnvironmentWithData(envs : List[AtomicEnvironment]) extends AtomicEnvironment{
  def referenceDataLookup = envs.head.referenceDataLookup

  def shiftsCanBeIgnored = envs.head.shiftsCanBeIgnored

  def setShiftsCanBeIgnored(canBeIgnored: Boolean)= UseMostRecentAtomicEnvironmentWithData(envs.map(_.setShiftsCanBeIgnored(canBeIgnored)))

  def marketDay = envs.head.marketDay

  def apply(key : AtomicDatumKey) = {
    def recurse(remainingEnvs : List[AtomicEnvironment]) : Any = {
      remainingEnvs match {
        case Nil => throw new MissingMarketDataException("No market data for " + key)
        case env :: rest => try {
          env(key)
        } catch {
          case e : MissingMarketDataException => recurse(rest)
        }
      }
    }
    recurse(envs)
  }
}

case class AllClosesEnvironmentRule(referenceDataLookup: ReferenceDataLookup) extends ClosesEnvironmentRule {
  val pricingGroups = List(PricingGroup.Metals)

  val label = EnvironmentRuleLabel.AllCloses


  def createEnv(observationDay: DayAndTime, marketDataReader: MarketDataReader): EnvironmentWithDomain = {
    val marketDay: DayAndTime = observationDay

    def read2(timeOfDay: ObservationTimeOfDay, key: MarketDataKey) : MarketData = {
      marketDataReader.read(TimedMarketDataKey(ObservationPoint(observationDay.day, timeOfDay), key))
    }
    val slice = marketDataSlice(marketDataReader, observationDay.day)

    val priceDataMap = Market.futuresMarketsView.safeMap { market =>
      val marketData = read2(market.closeTime, PriceDataKey(market))
      PriceDataKey(market) → marketData.asInstanceOf[PriceData]
    }.toMap

    val marketsX = {
      priceDataMap.toList.map { case (PriceDataKey(futuresMarket: FuturesMarket), priceData) => {
        UnderlyingDeliveryPeriods(futuresMarket.closeTime, futuresMarket, priceData.sortedKeys)
      }
      case (PriceDataKey(market: PublishedIndex), priceData) => {
        UnderlyingDeliveryPeriods(ObservationTimeOfDay.LondonClose, market, priceData.sortedKeys)
      } }
    }


    val environmentX = Environment(MarketDataCurveObjectEnvironment(marketDay, slice, false, referenceDataLookup))

    new EnvironmentWithDomain {
      val environment = environmentX
      def markets = marketsX
      override def discounts = marketDataReader.readAll(ForwardRateDataType.name, observationDay.day.atTimeOfDay(ObservationTimeOfDay.LiborClose)).map {
        case (key:ForwardRateDataKey, data:ForwardRateData) => key.ccy -> data
      }

      override def benchmarks = {
        marketDataReader.readAll(MarketDataTypeName.fromClass(classOf[CountryBenchmarkDataType]), ObservationPoint.RealTime).toList.map {
          case (k:CountryBenchmarkMarketDataKey,d:CountryBenchmarkData)=> k->d
        }
      }

      override def spotFX = marketDataReader.readAll(SpotFXDataType.name, observationDay.day.atTimeOfDay(ObservationTimeOfDay.LondonClose)).map {
        case (key:SpotFXDataKey, _) => key.ccy
      }
    }
  }
 }

 case class MostRecentClosesEnvironmentRule(referenceDataLookup: ReferenceDataLookup) extends ClosesEnvironmentRule {
   val pricingGroups = List(PricingGroup.Metals)

   val label = EnvironmentRuleLabel.MostRecentCloses
   private val numberOfDaysToLookBack = 14

   def createEnv(observationDay: DayAndTime, marketDataReader: MarketDataReader): EnvironmentWithDomain = {
     val marketDay: DayAndTime = observationDay

     def atomicEnvironments(marketDataDayAndTime: DayAndTime): AtomicEnvironment = {
       val slice = marketDataSlice(marketDataReader, marketDataDayAndTime.day)
       ForwardStateEnvironment(Undiscounted(MarketDataCurveObjectEnvironment(marketDataDayAndTime, slice, false, referenceDataLookup)), marketDay)
     }

     val observationDaysAndTimes = ((observationDay.day - numberOfDaysToLookBack) upto observationDay.day).filter(_.isWeekday).toList.flatMap{day => List(day.startOfDay, day.endOfDay)}
     val envs = observationDaysAndTimes.filterNot(_ > observationDay).sortWith(_ > _).map(atomicEnvironments(_))
//       flatMap(atomicEnvironments(_)).sortWith(_.marketDay > _.marketDay)
     val environmentX = Environment(CachingAtomicEnvironment(UseMostRecentAtomicEnvironmentWithData(envs)))

     new EnvironmentWithDomain {
       val environment = environmentX
       def markets = Nil
     }
   }
}
