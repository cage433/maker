package starling.metals.datasources


import starling.daterange._
import starling.db.MarketDataEntry
import starling.market._
import starling.marketdata._
import starling.quantity.{Quantity, UOM}
import starling.lim.{LIMConnection, LIMService}

import starling.utils.ImplicitConversions._
import LIMService._
import scalaz.Scalaz._
import collection.immutable.List


object LIBORFixingsSource extends LimSource {
  def description = TopRelation.Trafigura.Bloomberg.InterestRates.children.map(_.name + "TRAF.*.*.* (Close)")

  def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    val rates: NestedMap[Day, Relation, Double] = connection.getPrices(relations, Level.Close, start, end)

    val groupedRates: NestedMap3[TimedMarketDataKey, ForwardRateSource, Tenor, Quantity] =
      rates.mapNested { case (observationDay, relation, rate) =>
        relation.timedKey(observationDay) → (relation.source, (relation.tenor, Quantity(rate, UOM.PERCENT)))
      }.toNestedMap3

    groupedRates.map { case (timedKey, ratesForKey) => new MarketDataEntry(timedKey, ForwardRateData(ratesForKey)) }
  }

  private val relations = ForwardRateSource.values.flatMap { source =>
    List(source) ⊛ source.currencies.toList ⊛ source.tenors ⊛ List("%d%s", "%02d%s") apply(Relation.apply)
  }.distinctBy(_.toString)

  private case class Relation(source: ForwardRateSource, currency: UOM, tenor: Tenor, tenorFormat: String) {
    def timedKey(observationDay: Day) = ForwardRateDataKey(currency).onDay(observationDay)
    override def toString = "TRAF.%s.%s.%s" % (source, currency, tenor.format(tenorFormat))
  }
}