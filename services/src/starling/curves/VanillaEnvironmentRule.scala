package starling.curves

import starling.daterange._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.market.CommodityMarket
import starling.db.{MarketDataReaderMarketDataSlice, MarketDataReader}
import starling.gui.api.{PricingGroup, EnvironmentRuleLabel}


class VanillaEnvironmentRule(pointRule:(Day)=>ObservationPoint, timeOfDay:TimeOfDay, val label: EnvironmentRuleLabel,
  val pricingGroups: List[PricingGroup], referenceDataLookup: ReferenceDataLookup, dataTypes: MarketDataTypes) extends EnvironmentRule {

  override def createNullAtomicEnvironment(observationDay: Day) = new NullAtomicEnvironment(observationDay.atTimeOfDay(timeOfDay), ReferenceDataLookup.Null)

  def createEnv(observationDay: Day, marketDataReader: MarketDataReader) = {
    val observationPoint = pointRule(observationDay)

    val environmentX = {
      val slice = new MarketDataReaderMarketDataSlice(marketDataReader, observationPoint, dataTypes = dataTypes)
      val dayAndTime = observationDay.atTimeOfDay(timeOfDay)
      Environment(new MarketDataCurveObjectEnvironment(dayAndTime, slice, referenceDataLookup = referenceDataLookup))
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

      override def discounts = marketDataReader.readAll(ForwardRateDataType.name, observationPoint).map {
        case (key:ForwardRateDataKey, data:ForwardRateData) => key.ccy -> data
      }


      override def spotFX = marketDataReader.readAll(SpotFXDataType.name, observationPoint).map {
        case (key:SpotFXDataKey, _) => key.ccy
      }
    }
  }
}