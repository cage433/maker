package starling.metals.datasources

import collection.immutable.List

import starling.lim.LIMService
import starling.daterange._
import starling.db.MarketDataEntry
import starling.market._
import LIMService._
import starling.utils.Pattern._
import starling.marketdata._
import starling.quantity.{Quantity, UOM}
import starling.utils.ImplicitConversions._

object LIBORFixingsSource extends HierarchicalLimSource(TopRelation.Trafigura.Bloomberg.InterestRates.children, List(Level.Close)) {
  type Relation = LIBORRelation

  case class LIBORRelation(source: ForwardRateSource, currency: UOM, tenor: Tenor)

  def relationExtractor = Extractor.regex[Option[LIBORRelation]]("""TRAF\.(\w+)\.(\w+)\.(\w+)""") {
    case List(source, UOM.Parse(ccy), Tenor.Parse(tenor)) => Some(LIBORRelation(ForwardRateSource(source), ccy, tenor))
  }

  override def marketDataEntriesFrom(rates: List[Prices[LIBORRelation]]) = {
    rates.groupBy(group).map { case ((key, observationDay), grouped) =>
      MarketDataEntry(observationDay.atTimeOfDay(key.observationTime), key, ForwardRateData(toNestedMap(grouped)))
    }
  }

  def toNestedMap(rates: List[Prices[LIBORRelation]]): NestedMap[ForwardRateSource, Tenor, Quantity] = rates.map { rate =>
    (rate.relation.source, (rate.relation.tenor, Quantity(rate.priceByLevel(Level.Close), UOM.PERCENT)))
  }.toNestedMap

  def group(fixings: Prices[LIBORRelation]) = (ForwardRateDataKey(fixings.relation.currency), fixings.observationDay)
}