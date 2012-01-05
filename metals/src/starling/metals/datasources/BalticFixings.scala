package starling.metals.datasources


import starling.daterange._
import starling.db.MarketDataEntry
import starling.market._
import starling.pivot.MarketValue
import starling.quantity.UOM

import starling.lim.LIMService._
import starling.marketdata.{PriceFixingsHistoryData, PriceFixingsHistoryDataKey}
import starling.lim.LIMConnection
import collection.immutable.{Map, List}
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._


object BalticFixings extends LimSource(List(Level.Val)) {
  type Relation = BalticRelation

  def description = List(TopRelation.Energy.Tankers.BalticFreight.Index_Forward.name + "(Val)")

  case class BalticRelation(limPrefix: String, tenor: Tenor) {
    override def toString = "BALTIC.%s.%s" % (limPrefix, formattedTenor)
    def valueKey = (Level.Val, StoredFixingPeriod.tenor(Tenor.parse(formattedTenor))) // weird back-n-forth until we have a NamedTenor

    private def formattedTenor = tenor.tenorName.startsWith("CUR") ? tenor.tenorName | tenor.toString
  }

  override def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    val prices: NestedMap[CommodityMarket, Day, Map[BalticRelation, Double]] = relationsByMarket.mapValues {
      relations => connection.typedPrices(relations, Level.Val, start, end)
    }

    prices.mapNested { case (market, observationDay, pricesByRelation) =>
      MarketDataEntry(observationDay.atTimeOfDay(ObservationTimeOfDay.LondonClose),
        PriceFixingsHistoryDataKey(market),
        PriceFixingsHistoryData.create(pricesByRelation.map {
          case (relation, price) => relation.valueKey → MarketValue.quantity(price, UOM.USD / UOM.DAY)
        })
      )
    }.toList
  }

  private val tenors = Tenor.many("CAL", 1, 2, 3, 4, 5) :::
    (Tenor("CURMON", 0) :: Tenor.many("MON", 1, 2, 3, 4, 5)) :::
    (Tenor("CURQ",   0) :: Tenor.many("Q",   1, 2, 3, 4, 5)) ::: Tenor.many("HY", 1, 2)

  lazy val relationsByMarket: Map[CommodityMarket, List[BalticRelation]] = Map(
    "Baltic Supramax T/C Avg" → "SUPRAMAX.TC5",
    "Panamax T/C Average (Baltic)" → "PANAMAX.TC4",
    "Capesize T/C Average (Baltic)" → "CAPESIZE.TC4"
  ).mapKeys(Market.fromName).mapValues(limPrefix => tenors.map(BalticRelation(limPrefix, _)))
}