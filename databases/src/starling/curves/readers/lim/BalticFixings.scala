package starling.curves.readers.lim

import collection.immutable.List

import starling.daterange._
import starling.db.MarketDataEntry
import starling.market._
import starling.pivot.MarketValue
import starling.quantity.UOM

import starling.lim.LIMService._
import starling.utils.ImplicitConversions._
import starling.utils.Pattern._
import starling.marketdata.{PriceFixingsHistoryData, PriceFixingsHistoryDataKey}


object BalticFixings extends HierarchicalLimSource(List(TopRelation.Energy.Tankers.BalticFreight.Index_Forward), List(Level.Val)) {
  type Relation = BalticRelation

  val BalticMarket = Map(
    "SUPRAMAX.TC5" → "Baltic Supramax T/C Avg",
    "PANAMAX.TC4"  → "Panamax T/C Average (Baltic)",
    "CAPESIZE.TC4" → "Capesize T/C Average (Baltic)"
  ).mapValues(Market.fromName).toExtractor

  case class BalticRelation(market:CommodityMarket, tenor:Tenor) {
    val period = StoredFixingPeriod.tenor(tenor)
  }

  def relationExtractor = Extractor.regex[Option[BalticRelation]]("""BALTIC\.(\w+\.\w+)\.(\w+)""") {
    case List(BalticMarket(market), Tenor.Parse(tenor)) => Some(BalticRelation(market, tenor))
  }

  def marketDataEntriesFrom(fixings: List[Prices[BalticRelation]]) = {
    fixings.groupBy(group).map { case ((market, observationDay), grouped) =>
      MarketDataEntry(observationDay.atTimeOfDay(ObservationTimeOfDay.LondonClose),
        PriceFixingsHistoryDataKey(market),
        PriceFixingsHistoryData.create(grouped.flatMap(fixings => fixings.priceByLevel(Level.Val) match {
          case 0 => None
          case price => Some( (Level.Val, fixings.relation.period) → MarketValue.quantity(price, UOM.USD/UOM.DAY) )
        }))
      )
    }
  }

  private def group(fixings: Prices[BalticRelation]) = (fixings.relation.market, fixings.observationDay)
}