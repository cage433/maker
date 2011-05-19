package starling.services.trinity

import collection.immutable.Map

import starling.curves.readers.LIBORFixing
import starling.daterange._
import starling.db.{NormalMarketDataReader, MarketDataStore}
import starling.gui.api.{PricingGroup, MarketDataSelection}
import starling.market.Level
import starling.marketdata.{PriceFixingsHistoryData, PriceFixingsHistoryDataKey}
import starling.pivot.MarketValue
import starling.quantity.UOM

import Tenor._
import starling.curves.readers.LIBORFixing._
import starling.utils.ClosureUtil._
import starling.utils.ImplicitConversions._
import java.text.DecimalFormat


class XRTGenerator(marketDataStore: MarketDataStore) {
  def generate(observationDay: Day) = XRTGenerator.latestLiborFixings(marketDataStore, observationDay).values.flatMap {
    fixingsByTenor => fixingsByTenor.map { case (tenor, fixing) => {
      val maturityDay = fixing.maturityDay(tenor)
      "3301%s%s0000000000%s0000000000CN" % (fixing.currency, maturityDay.toString("YYMMdd"), fixing.format(XRTGenerator.format))
    }}
  }.toList
}

object XRTGenerator {
  val tenors = List(OneDay, OneWeek, TwoWeeks, OneMonth, TwoMonths, ThreeMonths, SixMonths, NineMonths, OneYear)
  val format = new DecimalFormat("000.000000")
  private val periods = tenors.map(tenor => (Level.Close, StoredFixingPeriod.tenor(tenor)))

  def latestLiborFixings(marketDataStore: MarketDataStore, observationDay: Day): Map[UOM, Map[Tenor, LIBORFixing]] =
    liborFixingsHistoryData(marketDataStore, observationDay).mapValues(_.fixingsFor(periods).collect {
      case ((Level.Close, StoredFixingPeriod.Tenor(tenor)), MarketValue.Quantity(quantity)) => {
        tenor → LIBORFixing(quantity, observationDay)
      }
    })

  private def liborFixingsHistoryData(marketDataStore: MarketDataStore, observationDay: Day): Map[UOM, PriceFixingsHistoryData] =
    currencies.toMapWithSomeValues(currency => safely(read(marketDataStore, observationDay, currency)).toOption)

  private def read(marketDataStore: MarketDataStore, observationDay: Day, currency: UOM) =
    latestLimOnlyMarketDataReader(marketDataStore).readAs[PriceFixingsHistoryData](
      observationDay.atTimeOfDay(ObservationTimeOfDay.LiborClose), PriceFixingsHistoryDataKey(currency.toString, Some("LIBOR")))

  private def latestLimOnlyMarketDataReader(marketDataStore: MarketDataStore) = new NormalMarketDataReader(
    marketDataStore, marketDataStore.latestMarketDataIdentifier(MarketDataSelection(Some(PricingGroup.LimOnly))))
}