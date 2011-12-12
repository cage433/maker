package starling.metals.datasources

import collection.immutable.List

import starling.daterange._
import starling.market._
import starling.marketdata._
import starling.pivot.MarketValue
import FuturesExchangeFactory._
import ObservationTimeOfDay._
import starling.utils.ImplicitConversions._
import starling.gui.api._
import starling.utils.{Log, Pattern}
import starling.scheduler.TaskDescription
import starling.quantity.UOM
import starling.db.{MarketDataStore, MarketDataEntry}
import starling.databases.{AbstractMarketDataProvider, PricingGroupMarketDataEventSource, MarketDataChange, MarketDataProvider}
import starling.lim.{LIMService, LIMConnection, LimNode}
import Level._
import LIMService.TopRelation._
import Pattern._
import scalaz.Scalaz._
import starling.services.EmailService
import com.lim.mimapi.RelType


object PriceFixingLimMarketDataSource {
  val sources = List(LMEFixings, BloombergTokyoCompositeFXRates, BalticFixings,
    new MonthlyFuturesFixings(Trafigura.Bloomberg.Futures.Shfe, FuturesExchangeFactory.SHFE.fixingLevel),

    // Import from LIM based source (i.e. not Bloomberg) temporarily,
    // I'm assuming that because Futures.Shfe.Aluminum.Close == Trafigura.Bloomberg.Futures.Shfe.Aluminium.Settle then
    //   Futures.Shfe.Lead.Close == the missing Trafigura.Bloomberg.Futures.Shfe.Lead.Settle  &&
    //   Futures.Shfe.Steel.Close == the missing Trafigura.Bloomberg.Futures.Shfe.Steel.Settle
    // These values are also brought in a prices (see PriceLimMarketDataSource)
    new MonthlyFuturesFixings(Futures.Shfe, Level.Close, Set(RelType.FUTURES), "") {
      override protected def marketPredicate(market: FuturesMarket) = market == Market.SHANGHAI_LEAD
    },

    new MonthlyFuturesFixings(Trafigura.Bloomberg.Futures.Comex, FuturesExchangeFactory.COMEX.fixingLevel)) ::: SpotFXFixings.all
}

case class PriceFixingLimMarketDataSource(service: LIMService, emailService: EmailService, template: Email)
  extends LimMarketDataSource(service, PriceFixingsHistoryDataType.name) {

  import PriceFixingLimMarketDataSource._

  override def description = descriptionFor(sources)
  def read(day: Day) = Map(getValuesForType(earliestDayToImport(day), day, sources))

  override def eventSources(marketDataStore: MarketDataStore) = {
    List(new PriceFixingDataEventSource(PricingGroup.Metals, ReferenceInterestMarketDataProvider(marketDataStore)))
  }

  override def availabilityTasks(marketDataStore: MarketDataStore) = List(
    TaskDescription("Verify LME AMR1 Fixings Available", limDaily(LME.calendar, 12 H 30), notImplemented),
  //      registerTasks(tasks(limDaily(LME, 13 H 15), TRAF.LME.{EUR, GBP, JPY} SpotFX
    TaskDescription("Verify LME OFFICIAL Fixings Available", limDaily(LME.calendar, 13 H 30), notImplemented),
    TaskDescription("Verify LME PMR1 LMEFixings Available", limDaily(LME.calendar, 16 H 30), notImplemented),
    TaskDescription("Verify LME UNOFFICIAL LMEFixings Available", limDaily(LME.calendar, 17 H 00), notImplemented),
    TaskDescription("Verify BloombergTokyoCompositeFXRates Available", limDaily(LME.calendar, 20 H 00), notImplemented),
    TaskDescription("Verify BalticFixings Available", limDaily(LME.calendar, 20 H 00), notImplemented),

    TaskDescription("Verify COMEX Fixings Available", limDaily(COMEX.calendar, 20 H 00), notImplemented),
    TaskDescription("Verify Shfe Fixings Available", limDaily(SHFE.calendar, 20 H 00), notImplemented)
  ).filterNot(_.task == notImplemented)
}

object LMEFixings extends LimSource(List(Ask, Bid)) {
  type Relation = LMEFixingRelation
  def description = List("%s/TRAF.LME.<commodity>.<ring>.<tenor> %s" % (Trafigura.Bloomberg.Metals.Lme, levelDescription))

  case class LMEFixingRelation(ring: ObservationTimeOfDay, market: CommodityMarket, tenor: Tenor)

  private val tenors = List(Tenor.CASH, Tenor(Month, 3), Tenor(Month, 15), Tenor(Month, 27))
  private val rings = List(LME_AMR1, LME_Official, LME_PMR1, LME_Unofficial)

  def relationsFrom(connection: LIMConnection) = for (market <- LME.markets; ring <- rings; tenor <- tenors) yield {
    (LMEFixingRelation(ring, market, tenor), "TRAF.LME.%S.%S.%s" % (market.commodity, ring, tenor))
  }

  def marketDataEntriesFrom(fixings: List[Prices[LMEFixingRelation]]) = {
    val groupedFixings = fixings.groupInto(keyGroup, fixing => (fixing.relation.tenor, fixing.priceByLevel))

    groupedFixings.map { case ((observationPoint, market), fixingsInGroup) =>
      val data = fixingsInGroup.flatMap { case (tenor, priceByLevel) => priceByLevel.map { case (level, price) =>
        (level, StoredFixingPeriod.tenor(tenor)) → MarketValue.quantity(price, market.priceUOM)
      } }

      MarketDataEntry(observationPoint, PriceFixingsHistoryDataKey(market), PriceFixingsHistoryData.create(data))
    }
  }

  private def keyGroup(fixing: Prices[LMEFixingRelation]) =
    (fixing.observationDay.atTimeOfDay(fixing.relation.ring), fixing.relation.market)
}

object BloombergTokyoCompositeFXRates extends HierarchicalLimSource(List(Trafigura.Bloomberg.Currencies.Composite), List(Close)) {
  type Relation = Rate
  case class Rate(currency: UOM, tenor: Tenor)

  def relationExtractor = Extractor.regex("""TRAF.CMPT.(\w+).(\w+)""")
    { case List(UOM.Parse(currency), Tenor.Parse(tenor)) => Some(Rate(currency, tenor)) }

  def marketDataEntriesFrom(fixings: List[Prices[Rate]]) = fixings.groupBy(keyGroup).map {
    case((currency, observationPoint), fixingsForCurrency) => {
      val prices = fixingsForCurrency.map { case (Prices(Rate(_, tenor), priceByLevel, _)) =>
        (Close, StoredFixingPeriod.tenor(tenor)) → MarketValue.quantity(priceByLevel(Close), currency / UOM.USD)
      }

      MarketDataEntry(observationPoint, PriceFixingsHistoryDataKey(currency.toString, Some("CMPT")),
        PriceFixingsHistoryData.create(prices))
    }
  }

  private def keyGroup(fixing: Prices[Rate]) = (fixing.relation.currency, fixing.observationDay.atTimeOfDay(SHFEClose))
}

class MonthlyFuturesFixings(override val parentNodes: List[LimNode], override val levels: List[Level],
  relationTypes: Set[RelType], prefix: String) extends HierarchicalLimSource(parentNodes, levels, relationTypes) with Log {

  def this(node: LimNode, level: Level, relationTypes: Set[RelType] = Set(RelType.CATEGORY), prefix: String = """TRAF\.""") =
    this(List(node), List(level), relationTypes, prefix)

  type Relation = MonthlyFuturesRelation

  case class MonthlyFuturesRelation(market: FuturesMarket, month: Month)

  protected def marketPredicate(market: FuturesMarket) = true

  def relationExtractor = Extractor.regex(prefix + """(\w+)\.(\w+)_(\w+)""") { case List(exchange, limSymbol, deliveryMonth) => {
    val optMarket = Market.fromExchangeAndLimSymbol(exchange, limSymbol).filter(marketPredicate)
    val optMonth = ReutersDeliveryMonthCodes.parse(deliveryMonth)

    (optMarket, optMonth) partialMatch { case (Some(market), Some(month)) => MonthlyFuturesRelation(market, month) }
  } }

  def marketDataEntriesFrom(fixings: List[Prices[MonthlyFuturesRelation]]) = {
    fixings.groupBy(group).map { case ((market, observationPoint), prices) => {
      val data = prices.flatMap { price => price.priceByLevel.map { case (level, priceAtLevel) => {
        val limMultiplier = market.limSymbol match {
          case Some(ls) => ls.multiplier
          case None => 1.0
        }
        (level, StoredFixingPeriod.dateRange(price.relation.month)) → MarketValue.quantity(priceAtLevel * limMultiplier, market.priceUOM)
      } } }

      MarketDataEntry(observationPoint, PriceFixingsHistoryDataKey(market), PriceFixingsHistoryData.create(data))
    } }
  }

  private def group(prices: Prices[MonthlyFuturesRelation]) =
    (prices.relation.market, prices.observationDay.atTimeOfDay(prices.relation.market.closeTime))
}

case class PriceFixingDataEventSource(pricingGroup: PricingGroup,
  provider: MarketDataProvider[(Day, String), UOM, PriceFixingsHistoryData]) extends PricingGroupMarketDataEventSource {

  type Key = (Day, String)
  type MarketType = UOM
  type CurveType = PriceFixingsHistoryData

  protected def marketDataEvent(change:MarketDataChange, key:(Day,String), marketTypes: List[UOM], snapshot:SnapshotIDLabel) = {
    Some(ReferenceInterestRateDataEvent(change.observationDay, key._2, marketTypes, snapshot, change.isCorrection))
  }

  protected def marketDataProvider = Some(provider)
}

case class ReferenceInterestMarketDataProvider(marketDataStore : MarketDataStore)
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