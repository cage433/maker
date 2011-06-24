package starling.curves

import starling.daterange._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.gui.api.EnvironmentRuleLabel
import starling.market.{CommodityMarket, FuturesMarket}
import starling.db.{MarketDataReaderMarketDataSlice, MarketDataReader}


class VanillaEnvironmentRule(
    pointRule:(Day)=>ObservationPoint,
    timeOfDay:TimeOfDay,
    val label: EnvironmentRuleLabel) extends EnvironmentRule {

  override def createNullAtomicEnvironment(observationDay: Day) = new NullAtomicEnvironment(observationDay.atTimeOfDay(timeOfDay))

  def createEnv(observationDay: Day, marketDataReader: MarketDataReader) = {
    val observationPoint = pointRule(observationDay)

    val environmentX = {
      val slice = new MarketDataReaderMarketDataSlice(marketDataReader, observationPoint)
      val dayAndTime = observationDay.atTimeOfDay(timeOfDay)
      Environment(new MarketDataCurveObjectEnvironment(dayAndTime, slice))
    }

    new EnvironmentWithDomain {
      val environment = environmentX
      def markets = {
        marketDataReader.readAllPrices(observationPoint).collect {
          case (PriceDataKey(market: CommodityMarket), priceData: PriceData)
            if market.tenor.isOneOf(Day, Month) && priceData.nonEmpty => {
              UnderlyingDeliveryPeriods(observationPoint.timeOfDay, market, priceData.sortedKeys)
            }
        }
      }
      override def marketVols= {
        marketDataReader.readAllVols(observationPoint).collect {
          case (OilVolSurfaceDataKey(market: CommodityMarket), data: OilVolSurfaceData)
            if market.tenor.isOneOf(Day, Month) && data.nonEmpty => {
              MarketOptionData(observationPoint.timeOfDay, market, data)
            }
        }
      }

      override def discounts = marketDataReader.readAll(ForwardRateDataType, observationPoint).map {
        case (key:ForwardRateDataKey, data:ForwardRateData) => key.ccy -> data
      }


      override def spotFX = marketDataReader.readAll(SpotFXDataType, observationPoint).map {
        case (key:SpotFXDataKey, _) => key.ccy
      }
    }
  }
}