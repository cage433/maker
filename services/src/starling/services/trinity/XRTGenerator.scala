package starling.services.trinity

import collection.immutable.Map
import java.text.DecimalFormat

import starling.daterange._
import starling.db.{NormalMarketDataReader, MarketDataStore}
import starling.gui.api.{PricingGroup, MarketDataSelection}
import starling.market.Level
import starling.marketdata.{TimedMarketDataKey, PriceFixingsHistoryData, PriceFixingsHistoryDataKey}
import starling.pivot.MarketValue
import Tenor._
import starling.curves.readers.lim.LIBORFixing._
import starling.utils.ClosureUtil._
import starling.utils.ImplicitConversions._
import starling.utils.Log
import starling.quantity.{Percentage, UOM}
import starling.curves.readers.lim.{LIBORFixings, LIBORFixing}


class XRTGenerator(marketDataStore: MarketDataStore) {
  def generate(observationDay: Day) = Nil
//    XRTGenerator.latestLiborFixings(marketDataStore, observationDay).values.flatMap {
//    fixingsByTenor => fixingsByTenor.map { case (tenor, fixing) => {
//      val maturityDay = fixing.maturityDay(tenor)
//      "3301%s%s0000000000%s0000000000CN" % (fixing.currency, maturityDay.toString("YYMMdd"), fixing.format(XRTGenerator.format))
//    }}
//  }.toList
}

object XRTGenerator {
  def tenorsFor(currency: UOM) = LIBORFixing.firstTenorFor(currency) :: commonTenors
//  val format = new DecimalFormat("000.000000")
  private val commonTenors = List(OneWeek, TwoWeeks, OneMonth, TwoMonths, ThreeMonths, SixMonths, NineMonths, TwelveMonths)
  private val periods = (ON :: SN :: commonTenors).map(tenor => (Level.Close, StoredFixingPeriod.tenor(tenor)))

  def latestLiborFixings(marketDataStore: MarketDataStore, observationDay: Day): NestedMap[UOM, Tenor, (Percentage, Day)] = {
    liborFixingsHistoryData(marketDataStore, observationDay).mapValues(_.fixingsFor(periods).collect {
      case ((Level.Close, StoredFixingPeriod.Tenor(tenor)), MarketValue.Percentage(percentage)) => {
        tenor → (percentage, observationDay)
      }
    })
  }

  private def liborFixingsHistoryData(marketDataStore: MarketDataStore, observationDay: Day): Map[UOM, PriceFixingsHistoryData] =
    currencies.toMapWithSomeValues(currency => safely(read(marketDataStore, observationDay, currency)).toOption)

  private def read(marketDataStore: MarketDataStore, observationDay: Day, currency: UOM) =
    latestLimOnlyMarketDataReader(marketDataStore).readAs[PriceFixingsHistoryData](TimedMarketDataKey(
      observationDay.atTimeOfDay(ObservationTimeOfDay.LiborClose), PriceFixingsHistoryDataKey(currency.toString, Some("LIBOR"))))

  private def latestLimOnlyMarketDataReader(marketDataStore: MarketDataStore) = new NormalMarketDataReader(
    marketDataStore, marketDataStore.latestMarketDataIdentifier(MarketDataSelection(Some(PricingGroup.Metals))))
}