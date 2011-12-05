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
  override def createNullAtomicEnvironment(observationDay: Day) = new NullAtomicEnvironment(observationDay.endOfDay, referenceDataLookup)

  def createEnv(observationDay: Day, marketDataReader: MarketDataReader): EnvironmentWithDomain = {
    val priceDataMap = Market.futuresMarkets.safeMap { market =>
      val marketData = marketDataReader. read(TimedMarketDataKey(ObservationPoint(observationDay, market.closeTime), PriceDataKey(market)))
      PriceDataKey(market) → marketData.asInstanceOf[PriceData]
    }.toMap

    val reader = new MarketDataSlice {
      def read(key: MarketDataKey) = read(key, observationDay)

      private def read(key: MarketDataKey, obsDay : Day) : MarketData = {
        key match {
          case priceDataKey@PriceDataKey(market) => {
            priceDataMap.getOrElse(priceDataKey, throw new MissingMarketDataException(
              "No " + market + " prices",
              "No " + market + " prices on " + observationDay + " at " + market.asInstanceOf[FuturesMarket].closeTime
            ))
          }
          case key: ForwardRateDataKey => marketDataReader.read(TimedMarketDataKey(observationDay.atTimeOfDay(ObservationTimeOfDay.Default), key))
          case key: CountryBenchmarkMarketDataKey => marketDataReader.read(TimedMarketDataKey(ObservationPoint.RealTime, key))
          case key: GradeAreaBenchmarkMarketDataKey => marketDataReader.read(TimedMarketDataKey(observationDay.atTimeOfDay(ObservationTimeOfDay.Default), key))
          case key: FreightParityDataKey =>  marketDataReader.read(TimedMarketDataKey(observationDay.atTimeOfDay(ObservationTimeOfDay.Default), key))
          case key: ShanghaiVATDataKey => marketDataReader.read(TimedMarketDataKey(ObservationPoint.RealTime, key))
          case key@SpotFXDataKey(UOM.CNY) => marketDataReader.read(TimedMarketDataKey(observationDay.atTimeOfDay(ObservationTimeOfDay.SHFEClose), key))
          case key: SpotFXDataKey => marketDataReader.read(TimedMarketDataKey(observationDay.atTimeOfDay(ObservationTimeOfDay.LondonClose), key))
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

    val environmentX = Environment(new MarketDataCurveObjectEnvironment(observationDay.endOfDay, reader, false, referenceDataLookup))

    new EnvironmentWithDomain {
      val environment = environmentX
      def markets = marketsX
      override def discounts = marketDataReader.readAll(ForwardRateDataType.name, observationDay.atTimeOfDay(ObservationTimeOfDay.LiborClose)).map {
        case (key:ForwardRateDataKey, data:ForwardRateData) => key.ccy -> data
      }

      override def spotFX = marketDataReader.readAll(SpotFXDataType.name, observationDay.atTimeOfDay(ObservationTimeOfDay.LondonClose)).map {
        case (key:SpotFXDataKey, _) => key.ccy
      }
    }
  }
}
