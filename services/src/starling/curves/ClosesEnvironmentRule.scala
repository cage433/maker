package starling.curves

import starling.daterange._
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.db.MarketDataReader
import starling.quantity.UOM
import starling.market._

import starling.gui.api.{PricingGroup, EnvironmentRuleLabel}

object ClosesEnvironmentRule {
  def label = EnvironmentRuleLabel.AllCloses
}

case class ClosesEnvironmentRule(referenceDataLookup: ReferenceDataLookup) extends EnvironmentRule {
  val pricingGroups = List(PricingGroup.Metals)

  val label = ClosesEnvironmentRule.label
  override def createNullAtomicEnvironment(observationDay: DayAndTime) = new NullAtomicEnvironment(observationDay, referenceDataLookup)

  def createEnv(observationDay: DayAndTime, marketDataReader: MarketDataReader): EnvironmentWithDomain = {
    val priceDataMap = Market.futuresMarkets.safeMap { market =>
      val marketData = marketDataReader. read(TimedMarketDataKey(ObservationPoint(observationDay.day, market.closeTime), PriceDataKey(market)))
      PriceDataKey(market) → marketData.asInstanceOf[PriceData]
    }.toMap

    val reader = new MarketDataSlice {
      def read(key: MarketDataKey) = read(key, observationDay.day)

      private def read(key: MarketDataKey, obsDay : Day) : MarketData = {
        key match {
          case priceDataKey@PriceDataKey(market) => {
            priceDataMap.getOrElse(priceDataKey, throw new MissingMarketDataException(
              "No " + market + " prices",
              "No " + market + " prices on " + observationDay + " at " + market.asInstanceOf[FuturesMarket].closeTime
            ))
          }
          case key: ForwardRateDataKey => marketDataReader.read(TimedMarketDataKey(observationDay.day.atTimeOfDay(key.observationTime), key))
          case key: CountryBenchmarkMarketDataKey => marketDataReader.read(TimedMarketDataKey(ObservationPoint.RealTime, key))
          case key: FreightParityDataKey =>  marketDataReader.read(TimedMarketDataKey(observationDay.day.atTimeOfDay(ObservationTimeOfDay.Default), key))
          case key: ShanghaiVATDataKey => marketDataReader.read(TimedMarketDataKey(ObservationPoint.RealTime, key))
          case key@SpotFXDataKey(UOM.CNY) => marketDataReader.read(TimedMarketDataKey(observationDay.day.atTimeOfDay(ObservationTimeOfDay.SHFEClose), key))
          case key: SpotFXDataKey => marketDataReader.read(TimedMarketDataKey(observationDay.day.atTimeOfDay(ObservationTimeOfDay.LondonClose), key))
          case _ => throw new Exception(name + " Closes Rule has no rule for " + key)
        }
      }

      def fixings(key : PriceFixingsHistoryDataKey, observationPoint: ObservationPoint): PriceFixingsHistoryData = {
        key.read(observationPoint, marketDataReader)
      }
    }

    val marketsX = {
      priceDataMap.toList.map { case (PriceDataKey(futuresMarket: FuturesMarket), priceData) => {
        UnderlyingDeliveryPeriods(futuresMarket.closeTime, futuresMarket, priceData.sortedKeys)
      }
      case (PriceDataKey(market: PublishedIndex), priceData) => {
        UnderlyingDeliveryPeriods(ObservationTimeOfDay.LondonClose, market, priceData.sortedKeys)
      } }
    }

    val environmentX = Environment(new MarketDataCurveObjectEnvironment(observationDay, reader, false, referenceDataLookup))

    new EnvironmentWithDomain {
      val environment = environmentX
      def markets = marketsX
      override def discounts = marketDataReader.readAll(ForwardRateDataType.name, observationDay.day.atTimeOfDay(ObservationTimeOfDay.LiborClose)).map {
        case (key:ForwardRateDataKey, data:ForwardRateData) => key.ccy -> data
      }

      override def spotFX = marketDataReader.readAll(SpotFXDataType.name, observationDay.day.atTimeOfDay(ObservationTimeOfDay.LondonClose)).map {
        case (key:SpotFXDataKey, _) => key.ccy
      }
    }
  }
}
