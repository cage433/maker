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
import starling.utils.ImplicitConversions._


object SpotFXFixings {
  import UOM._;
  import Trafigura.Bloomberg._

  val all = List(new SpotFXFixings("ECB", ECBPublicationTime, Spot, EUR, "ECB", ForeignExchange.Ecb),
    new SpotFXFixings("LME", LMEClose, Close, USD, "TRAF.LME.", Currencies.LME, Currencies.Lme), CFETSSpotFXFixings)
}

class SpotFXFixings(exchange: String, timeOfDay: ObservationTimeOfDay, level: Level, against: UOM, prefix: String, nodes: LimNode*)
  extends LimSource(List(level)) {

  type Relation = Nothing

  def description = nodes.map(node => "%s %s.* (%s)" % (node.name, prefix, level)).toList

  override def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    UOM.currencies.toMapWithValues { currency => connection.getPrices(prefix + currency, level, start, end) }
      .mapNested { case (currency, observationDay, fixing) =>
        MarketDataEntry(observationDay.atTimeOfDay(timeOfDay), key(currency), value(fixing, currency))
      }.toList
  }

  private def key(currency: UOM): MarketDataKey = PriceFixingsHistoryDataKey(currency.toString, Some(exchange))
  private def value(price: Double, currency: UOM): MarketData = PriceFixingsHistoryData.create(
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