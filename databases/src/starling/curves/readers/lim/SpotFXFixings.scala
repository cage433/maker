package starling.curves.readers.lim

import collection.immutable.List

import starling.daterange._
import starling.db.MarketDataEntry
import starling.market._
import starling.marketdata._
import starling.quantity.{Quantity, UOM}
import starling.utils.Pattern

import Level._
import starling.lim.{LIMService, LimNode}
import LIMService.TopRelation._
import ObservationTimeOfDay._
import Pattern._


object SpotFXFixings {
  import UOM._;
  import Trafigura.Bloomberg._

  val all = List(new SpotFXFixings("ECB", ECBPublicationTime, Spot, EUR, """ECB(\w+)""", ForeignExchange.Ecb),
    new SpotFXFixings("LME", LMEClose, Close, USD, """TRAF\.LME\.(\w+)""", Currencies.LME, Currencies.Lme))
}

class SpotFXFixings(exchange: String, timeOfDay: ObservationTimeOfDay, level: Level, against: UOM, regex: String, nodes: LimNode*)
  extends HierarchicalLimSource(nodes.toList, List(level)) {

  type Relation = UOM
  def relationExtractor = Extractor.regex(regex) { case List(UOM.Parse(currency)) => Some(currency) }

  def marketDataEntriesFrom(prices: List[Prices[UOM]]) = prices.map { case Prices(currency, priceByLevel, observationDay) =>
    MarketDataEntry(observationDay.atTimeOfDay(timeOfDay), key(currency), value(priceByLevel(level), currency))
  }

  protected def key(currency: UOM): MarketDataKey = PriceFixingsHistoryDataKey(currency.toString, Some(exchange))

  protected def value(price: Double, currency: UOM): MarketData = PriceFixingsHistoryData.create(
    level, StoredFixingPeriod.tenor(Tenor.OneDay), (Quantity(price, currency / against)))
}