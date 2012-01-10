package starling.metals.datasources

import collection.immutable.List

import starling.daterange._
import starling.db.MarketDataEntry
import starling.market._
import starling.marketdata._
import starling.quantity.{Quantity, UOM}, UOM._

import starling.lim.{LIMConnection, LIMService, LimNode}, LIMService.TopRelation._
import Level._
import ObservationTimeOfDay._
import starling.utils.ImplicitConversions._


object SpotFXFixings {
  import UOM._;
  import Trafigura.Bloomberg._

  val all = List(new SpotFXFixings("ECB", ECBPublicationTime, Spot, EUR, "ECB", ForeignExchange.Ecb),
    new SpotFXFixings("LME", LMEClose, Close, USD, "TRAF.LME.", Currencies.LME, Currencies.Lme), CFETSSpotFXFixings)
}

class SpotFXFixings(exchange: String, timeOfDay: ObservationTimeOfDay, level: Level, against: UOM, prefix: String,
  nodes: LimNode*) extends LimSource {

  def description = nodes.map(node => "%s %s.* (%s)" % (node.name, prefix, level)).toList

  def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    connection.getPrices(UOM.currencies.map(Relation), level, start, end).mapNested {
      case (observationDay, relation, fixing) =>
        MarketDataEntry(observationDay.atTimeOfDay(timeOfDay), relation.key, relation.fixingFor(fixing))
    }
  }

  private case class Relation(currency: UOM) {
    override def toString = prefix + currency
    def key = PriceFixingsHistoryDataKey(currency.toString, Some(exchange))

    def fixingFor(value: Double) = PriceFixingsHistoryData.create(
      level, StoredFixingPeriod.tenor(Tenor.OneDay), (Quantity(value, currency / against)))
  }
}


object CFETSSpotFXFixings extends LimSource {
  def description = List(Trafigura.Bloomberg.Currencies.Composite.name + " TRAF.CFETS.CNY (Close)")

  def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    connection.getPrices("TRAF.CFETS.CNY", Close, start, end).map { case (observationDay, fixing) =>
      MarketDataEntry(observationDay.atTimeOfDay(SHFEClose), SpotFXDataKey(CNY), SpotFXData(Quantity(fixing, CNY / USD)))
    }
  }
}