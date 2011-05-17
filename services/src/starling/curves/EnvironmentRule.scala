package starling.curves

import starling.daterange._
import starling.market.FuturesExchangeFactory._
import collection.immutable.List
import starling.utils.ImplicitConversions._
import starling.gui.api.EnvironmentRuleLabel
import starling.marketdata.{PriceFixingsHistoryDataKey, MarketDataKey}
import starling.market.{CommodityMarket, FuturesMarket}
import starling.db.{MarketDataReaderMarketDataSlice, MarketDataReader}


trait EnvironmentRule {
  val label: EnvironmentRuleLabel
  def name: String = label.name
  def createEnv(observationDay: Day, marketDataReader: MarketDataReader): EnvironmentWithDomain
}

object EnvironmentRule {
  import ObservationTimeOfDay._

  private val metalRules = List(
    ClosesEnvironmentRule,
    new VanillaEnvironmentRule((day)=>ObservationPoint(day, SHFEClose), TimeOfDay.EndOfDay, new EnvironmentRuleLabel(SHFEClose.name)),
    TimeShiftToLMECloseEnvironmentRule
  )
  val Default = new VanillaEnvironmentRule((day)=>ObservationPoint(day, ObservationTimeOfDay.Default), TimeOfDay.EndOfDay, EnvironmentRuleLabel.COB)
  val RealTime = new VanillaEnvironmentRule((day)=>ObservationPoint.RealTime, TimeOfDay.StartOfDay, EnvironmentRuleLabel.RealTime)
  private val defaultRules = List(Default, RealTime)
  private val lookup = (metalRules ::: defaultRules).asMap(_.label)

  val exchangeCloses = Map(SFS → SHFEClose, LME → LMEClose, COMEX → COMEXClose)
  val marketCloses = exchangeCloses.extendKey((market: FuturesMarket) => market.exchange)
  val metalsRulesLabels = metalRules.map(_.label)
  val defaultRulesLabels = defaultRules.map(_.label)

  def forName(name: String): EnvironmentRule = forLabel(EnvironmentRuleLabel(name))
  def forLabel(rule: EnvironmentRuleLabel): EnvironmentRule = lookup(rule)
}