package starling.metals.datasources

import collection.immutable.List

import starling.daterange._
import starling.db.MarketDataEntry
import starling.market._
import starling.marketdata._
import starling.quantity.{Quantity, UOM}, UOM._
import starling.utils.Pattern

import starling.lim.{LIMConnection, LIMService, LimNode}
import Level._
import Pattern._
import LIMService.TopRelation._
import ObservationTimeOfDay._


object SpotFXFixings {
  import UOM._;
  import Trafigura.Bloomberg._

  val all = List(new SpotFXFixings("ECB", ECBPublicationTime, Spot, EUR, """ECB(\w+)""", ForeignExchange.Ecb),
    new SpotFXFixings("LME", LMEClose, Close, USD, """TRAF\.LME\.(\w+)""", Currencies.LME, Currencies.Lme), CFETSSpotFXFixings)
}

class SpotFXFixings(exchange: String, timeOfDay: ObservationTimeOfDay, level: Level, against: UOM, regex: String, nodes: LimNode*)
  extends HierarchicalLimSource(nodes.toList, List(level)) {

  type Relation = UOM
  def relationExtractor = Extractor.regex(regex) { case List(UOM.Parse(currency)) => Some(currency) }

  override def marketDataEntriesFrom(prices: List[Prices[UOM]]) = prices.map { case Prices(currency, priceByLevel, observationDay) =>
    MarketDataEntry(observationDay.atTimeOfDay(timeOfDay), key(currency), value(priceByLevel(level), currency))
  }

  protected def key(currency: UOM): MarketDataKey = PriceFixingsHistoryDataKey(currency.toString, Some(exchange))

  protected def value(price: Double, currency: UOM): MarketData = PriceFixingsHistoryData.create(
    level, StoredFixingPeriod.tenor(Tenor.OneDay), (Quantity(price, currency / against)))
}

object CFETSSpotFXFixings extends LimSource(List(Close)) {
  type Relation = Nothing

  def description = List(Trafigura.Bloomberg.Currencies.Composite.name + " TRAF.CFETS.CNY (Close)")

  override def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    connection.getPrices("TRAF.CFETS.CNY", Close, start, end).map { case (observationDay, fixing) =>
      MarketDataEntry(observationDay.atTimeOfDay(SHFEClose), SpotFXDataKey(CNY), SpotFXData(Quantity(fixing, CNY / USD)))
    }.toList
  }
}