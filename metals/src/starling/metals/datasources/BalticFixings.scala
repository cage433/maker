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


object BalticFixings extends LimSource {
  def description = List(TopRelation.Energy.Tankers.BalticFreight.Index_Forward.name + "(Val)")

  def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    val prices: NestedMap3[CommodityMarket, Day, Relation, Double] =
      relationsByMarket.mapValues(connection.getPrices(_, Level.Val, start, end))

    prices.mapNested { case (market, observationDay, pricesByRelation) =>
      MarketDataEntry(observationDay.atTimeOfDay(ObservationTimeOfDay.LondonClose), PriceFixingsHistoryDataKey(market),
        PriceFixingsHistoryData.create(pricesByRelation.map {
          case (relation, price) => relation.valueKey → MarketValue.quantity(price, UOM.USD / UOM.DAY)
        })
      )
    }
  }

  private val tenors = Tenor.many("CAL", 1, 2, 3, 4, 5) :::
    (Tenor("CURMON", 0) :: Tenor.many("MON", 1, 2, 3, 4, 5)) :::
    (Tenor("CURQ",   0) :: Tenor.many("Q",   1, 2, 3, 4, 5)) ::: Tenor.many("HY", 1, 2)

  private lazy val relationsByMarket: Map[CommodityMarket, List[Relation]] = Map(
    "Baltic Supramax T/C Avg" → "SUPRAMAX.TC5",
    "Panamax T/C Average (Baltic)" → "PANAMAX.TC4",
    "Capesize T/C Average (Baltic)" → "CAPESIZE.TC4"
  ).mapKeys(Market.fromName).mapValues(limPrefix => tenors.map(Relation(limPrefix, _)))

  private case class Relation(limPrefix: String, tenor: Tenor) {
    override def toString = "BALTIC.%s.%s" % (limPrefix, formattedTenor)
    def valueKey = (Level.Val, StoredFixingPeriod.tenor(Tenor.parse(formattedTenor))) // weird back-n-forth until we have a NamedTenor

    private def formattedTenor = tenor.tenorName.startsWith("CUR") ? tenor.tenorName | tenor.toString
  }
}