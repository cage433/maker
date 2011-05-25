package starling.curves

import starling.daterange._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.gui.api.EnvironmentRuleLabel
import starling.db.MarketDataReader
import starling.market.{CommodityMarket, Market, FuturesMarket}


object ClosesEnvironmentRule extends EnvironmentRule {
  import EnvironmentRule._

  val label = EnvironmentRuleLabel("All Closes")
  lazy val marketsWithCloseTimeOfDay = Market.futuresMarkets.optPair(marketCloses.get(_)).toList

  def createEnv(observationDay: Day, marketDataReader: MarketDataReader): EnvironmentWithDomain = {
    val priceDataMap = marketsWithCloseTimeOfDay.flatMap {
      case (market, timeOfDay) => try {
        val marketData = marketDataReader.read(ObservationPoint(observationDay, timeOfDay), PriceDataKey(market))
        Some(PriceDataKey(market) → marketData.asInstanceOf[PriceData])
      } catch {
        case e: MissingMarketDataException => None
      }
    }.toMap

    val reader = new MarketDataSlice {
      def read(key: MarketDataKey) = {
        key match {
          case priceDataKey @ PriceDataKey(futuresMarket: FuturesMarket) => {
            if (!marketCloses.contains(futuresMarket)) {
              throw new Exception("No close rule for " + futuresMarket.exchange)
            }

            priceDataMap.getOrElse(priceDataKey,
              throw new Exception("No price for " + futuresMarket + "@" + marketCloses.contains(futuresMarket)))
          }
          case key:ForwardRateDataKey => marketDataReader.read(observationDay.atTimeOfDay(ObservationTimeOfDay.Default), key)
          case key:SpotFXDataKey => marketDataReader.read(observationDay.atTimeOfDay(ObservationTimeOfDay.LondonClose), key)
          case _ => throw new Exception(name + " only has rules for futures Prices")
        }
      }

      def fixings(market: CommodityMarket, observationPoint: ObservationPoint) = throw new Exception("Not implemented")
    }

    val marketsX = {
      priceDataMap.toList.map { case (PriceDataKey(futuresMarket: FuturesMarket), priceData) => {
        MarketDeliveryPeriods(marketCloses(futuresMarket), futuresMarket, priceData.sortedKeys)
      }}
    }

    val environmentX = Environment(new MarketDataCurveObjectEnvironment(new DayAndNoTime(observationDay), reader))

    new EnvironmentWithDomain {
      val environment = environmentX
      def markets = marketsX
      override def discounts = marketDataReader.readAll(ForwardRateDataType, observationDay.atTimeOfDay(ObservationTimeOfDay.Default)).map {
        case (key:ForwardRateDataKey, data:ForwardRateData) => key.ccy -> data
      }

      override def spotFX = marketDataReader.readAll(SpotFXDataType, observationDay.atTimeOfDay(ObservationTimeOfDay.LondonClose)).map {
        case (key:SpotFXDataKey, _) => key.ccy
      }
    }
  }
}