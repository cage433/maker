package starling.curves

import starling.daterange._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.gui.api.EnvironmentRuleLabel
import starling.db.MarketDataReader
import starling.quantity.UOM
import starling.calendar.BusinessCalendar
import starling.pivot.MarketValue
import starling.market._


object ClosesEnvironmentRule extends EnvironmentRule {
  import EnvironmentRule._

  val label = EnvironmentRuleLabel.AllCloses
  lazy val marketsWithCloseTimeOfDay = Market.futuresMarkets.optPair(marketCloses.get(_)).toList

  override def createNullAtomicEnvironment(observationDay: Day) = new NullAtomicEnvironment(observationDay.endOfDay)

  def createEnv(observationDay: Day, marketDataReader: MarketDataReader): EnvironmentWithDomain = {
    val priceDataMap = marketsWithCloseTimeOfDay.flatMap {
      case (market, timeOfDay) => try {
        val marketData = marketDataReader.read(TimedMarketDataKey(observationDay.atTimeOfDay(timeOfDay), PriceDataKey(market)))
        Some(PriceDataKey(market) → marketData.asInstanceOf[PriceData])
      } catch {
        case e: MissingMarketDataException => None
      }
    }.toMap ++ FreightCurve.freightPriceData(observationDay, marketDataReader)

    val reader = new MarketDataSlice {
      def read(key: MarketDataKey) = {
        key match {
          case priceDataKey @ PriceDataKey(market) => {
            priceDataMap.getOrElse(priceDataKey, throw new Exception("No prices for " + market))
          }
          case key:ForwardRateDataKey => marketDataReader.read(TimedMarketDataKey(observationDay.atTimeOfDay(ObservationTimeOfDay.Default), key))
          case key@SpotFXDataKey(UOM.CNY) => marketDataReader.read(TimedMarketDataKey(observationDay.atTimeOfDay(ObservationTimeOfDay.SHFEClose), key))
          case key:SpotFXDataKey => marketDataReader.read(TimedMarketDataKey(observationDay.atTimeOfDay(ObservationTimeOfDay.LondonClose), key))
          case _ => throw new Exception(name + " Closes Rule has no rule for " + key)
        }
      }

      def fixings(key : PriceFixingsHistoryDataKey, observationPoint: ObservationPoint): PriceFixingsHistoryData = key.read(observationPoint, marketDataReader)
    }

    val marketsX = {
      priceDataMap.toList.map { case (PriceDataKey(futuresMarket: FuturesMarket), priceData) => {
        UnderlyingDeliveryPeriods(marketCloses(futuresMarket), futuresMarket, priceData.sortedKeys)
      }
      case (PriceDataKey(market: PublishedIndex), priceData) => {
        UnderlyingDeliveryPeriods(ObservationTimeOfDay.LondonClose, market, priceData.sortedKeys)
      } }
    }

    val environmentX = Environment(new MarketDataCurveObjectEnvironment(observationDay.endOfDay, reader))

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