package starling.metals.datasources


import starling.daterange._
import starling.db.MarketDataEntry
import starling.market._
import starling.marketdata._
import starling.quantity.{Quantity, UOM}
import starling.lim.{LIMConnection, LIMService}
import collection.immutable.List

import starling.utils.ImplicitConversions._
import LIMService._
import scalaz.Scalaz._


object LIBORFixingsSource extends LimSource(List(Level.Close)) {
  type Relation = Nothing

  def description = TopRelation.Trafigura.Bloomberg.InterestRates.children.map(_.name + "TRAF.*.*.* (Close)")

  override def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    val groupedRates = ForwardRateSource.values.flatMap(getPrices(connection, start, end, _)).toMultiMap

    groupedRates.map { case ((key, observationDay), ratesForGroup) =>
      MarketDataEntry(observationDay.atTimeOfDay(key.observationTime), key, ForwardRateData(ratesForGroup.toNestedMap))
    }.toList
  }

  private def getPrices(connection: LIMConnection, start: Day, end: Day, source: ForwardRateSource) = {
    def getPrices(currency: UOM, tenor: Tenor): Iterable[((ForwardRateDataKey, Day), (ForwardRateSource, (Tenor, Quantity)))] = {
      val relations = List(tenor.format("%d%s"), tenor.format("%02d%s"))
        .map(formattedTenor => "TRAF.%s.%s.%s" % (source, currency, formattedTenor))

      connection.getPrices(relations, Level.Close, start, end).map { case ((_, observationDay), rate) =>
        (ForwardRateDataKey(currency), observationDay) → (source, (tenor, Quantity(rate, UOM.PERCENT)))
      }
    }

    (source.currencies.toList ⊛ source.tenors apply(getPrices)).flatten
  }
}