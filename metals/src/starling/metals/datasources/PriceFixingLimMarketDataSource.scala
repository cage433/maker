package starling.metals.datasources


import starling.daterange._
import starling.market._
import starling.marketdata._
import starling.pivot.MarketValue
import FuturesExchangeFactory._
import ObservationTimeOfDay._
import starling.utils.ImplicitConversions._
import starling.gui.api._
import starling.scheduler.TaskDescription
import starling.db.{MarketDataStore, MarketDataEntry}
import starling.databases.{AbstractMarketDataProvider, PricingGroupMarketDataEventSource, MarketDataChange, MarketDataProvider}
import starling.lim.{LIMService, LIMConnection, LimNode}
import Level._
import LIMService.TopRelation._
import starling.services.EmailService
import com.lim.mimapi.RelType
import starling.quantity.{Quantity, UOM}
import starling.utils.{ImplicitConversions, Log, Pattern}
import Pattern._
import scalaz.Scalaz._
import collection.immutable.{Map, List}


object PriceFixingLimMarketDataSource {
  val sources = List(LMEFixings, BloombergTokyoCompositeFXRates, BalticFixings,
    new MonthlyFuturesFixings(Trafigura.Bloomberg.Futures.Shfe, FuturesExchangeFactory.SHFE),
    new MonthlyFuturesFixings(Trafigura.Bloomberg.Futures.Comex, FuturesExchangeFactory.COMEX)) ::: SpotFXFixings.all
}

case class PriceFixingLimMarketDataSource(service: LIMService, emailService: EmailService, template: Email)
  extends LimMarketDataSource(service, PriceFixingsHistoryDataType.name) {

  import PriceFixingLimMarketDataSource._

  override def description = descriptionFor(sources)
  def read(day: Day) = Map(getValuesForType(earliestDayToImport(day), day, sources))

  override def availabilityTasks(marketDataStore: MarketDataStore) = List(
    TaskDescription("Verify LME AMR1 Fixings Available", limDaily(LME.calendar, 12 H 30), notImplemented),
    TaskDescription("Verify LME OFFICIAL Fixings Available", limDaily(LME.calendar, 13 H 30), notImplemented),
    TaskDescription("Verify LME PMR1 LMEFixings Available", limDaily(LME.calendar, 16 H 30), notImplemented),
    TaskDescription("Verify LME UNOFFICIAL LMEFixings Available", limDaily(LME.calendar, 17 H 00), notImplemented),
    TaskDescription("Verify BloombergTokyoCompositeFXRates Available", limDaily(LME.calendar, 20 H 00), notImplemented),
    TaskDescription("Verify BalticFixings Available", limDaily(LME.calendar, 20 H 00), notImplemented),

    TaskDescription("Verify COMEX Fixings Available", limDaily(COMEX.calendar, 20 H 00), notImplemented),
    TaskDescription("Verify Shfe Fixings Available", limDaily(SHFE.calendar, 20 H 00), notImplemented)
  ).filterNot(_.task == notImplemented)
}

object LMEFixings extends LimSource {
  def description = List(Trafigura.Bloomberg.Metals.Lme.name + "/TRAF.LME.<commodity>.<ring>.<tenor> (Ask, Bid)")

  def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    val fixings: NestedMap3[Day, Relation, Level, Double] = connection.getPrices(relations, List(Ask, Bid), start, end)

    val groupedFixings = fixings.mapNested { case (observationDay, relation, fixingsByLevel) =>
      ((observationDay.atTimeOfDay(relation.ring), relation.market), (relation.tenor, fixingsByLevel))
    }.toNestedMap

    groupedFixings.map { case ((observationPoint, market), fixingsInGroup) =>
      val data = fixingsInGroup.mapNested { case (tenor, level, price) =>
        (level, StoredFixingPeriod.tenor(tenor)) → MarketValue.quantity(price, market.priceUOM)
      }

      MarketDataEntry(observationPoint, PriceFixingsHistoryDataKey(market), PriceFixingsHistoryData.create(data))
    }
  }

  private val tenors = Tenor.CASH :: Tenor.many(Month, 3, 15, 27)
  private val rings = List(LME_AMR1, LME_Official, LME_PMR1, LME_Unofficial)
  private val relations: List[Relation] = rings ⊛ LME.markets ⊛ tenors apply(Relation.apply)

  private case class Relation(ring: ObservationTimeOfDay, market: CommodityMarket, tenor: Tenor) {
    override def toString = "TRAF.LME.%S.%S.%s" % (market.commodity.limName, ring.shortName, tenor)
  }
}

object BloombergTokyoCompositeFXRates extends LimSource {
  def description = List(Trafigura.Bloomberg.Currencies.Composite.name + "TRAF.CMPT.CNY.* (Close)")

  def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day) = {
    val rates: NestedMap[Day, Relation, Double] = connection.getPrices(relations, Close, start, end)

    rates.map { case (observationDay, ratesByRelation) =>
      MarketDataEntry(observationDay.atTimeOfDay(SHFEClose), PriceFixingsHistoryDataKey("CNY", Some("CMPT")),
        PriceFixingsHistoryData.create(ratesByRelation.map {
          case (relation, rate) => ((Close, relation.period), MarketValue.quantity(rate, UOM.CNY / UOM.USD))
        })
      )
    }
  }

  private def relations = (Tenor.many(Month, 1, 2, 3, 6, 9, 12) ++ Tenor.many(Year, 2, 3, 4)).map(Relation)

  private case class Relation(tenor: Tenor) {
    override def toString = "TRAF.CMPT.CNY." + period
    val period = StoredFixingPeriod.tenor(tenor)
  }
}

case class BrokenReferenceInterestMarketDataProvider(marketDataStore : MarketDataStore)
  extends AbstractMarketDataProvider[(Day, String), UOM, PriceFixingsHistoryData](marketDataStore) {

  val marketDataType = PriceFixingsHistoryDataType.name
  val marketDataKeys = None

  def marketDataFor(timedData: List[(TimedMarketDataKey, MarketData)]) = {
    val fixings: List[(Option[Day], Option[String], String, PriceFixingsHistoryData)] = timedData.map { case (timedKey, data) => {
      val key = timedKey.key.asInstanceOf[PriceFixingsHistoryDataKey]
      val fixingsForKey = data.asInstanceOf[PriceFixingsHistoryData]

      (timedKey.day, key.exchangeName, key.marketName, fixingsForKey)
    } }

    // (LIBOR, EUR, Map((Level.Close, ON) → Quantity(0.123)))
    val fixingsByDayAndExchange: List[((Day, String), (UOM, PriceFixingsHistoryData))] = fixings.collect {
      case (Some(observationDay), Some(exchangeName), UOM.Currency(currency), fixingsForDayAndExchange) =>
        ((observationDay, exchangeName), (currency, fixingsForDayAndExchange))
    }

    fixingsByDayAndExchange.groupBy(_._1).mapValues(_.map(_._2).toMap)
  }
}