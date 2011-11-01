package starling.metals.datasources

import collection.immutable.List

import starling.daterange._
import starling.market._
import starling.marketdata._
import starling.pivot.MarketValue
import FuturesExchangeFactory._
import ObservationTimeOfDay._
import starling.utils.ImplicitConversions._
import starling.scheduler.ScheduledTime._
import starling.gui.api._
import starling.utils.{Broadcaster, Pattern}
import starling.scheduler.{EmailingScheduledTask, TaskDescription}
import starling.quantity.{Percentage, UOM}
import starling.utils.ClosureUtil._
import starling.db.{NormalMarketDataReader, MarketDataStore, MarketDataEntry}
import starling.databases.{AbstractMarketDataProvider, PricingGroupMarketDataEventSource, MarketDataChange, MarketDataProvider}
import starling.lim.{LIMService, LIMConnection, LimNode}
import Level._
import LIMService.TopRelation._
import Pattern._
import scalaz.Scalaz._
import starling.calendar.{BusinessCalendars}
import starling.services.EmailService


object PriceFixingLimMarketDataSource {
  val fixingsSources = PriceFixingsHistoryDataType.name → (List(LMEFixings, LIBORFixings, BloombergTokyoCompositeFXRates, BalticFixings,
    new MonthlyFuturesFixings(Trafigura.Bloomberg.Futures.Shfe, FuturesExchangeFactory.SFS.fixingLevel),
    new MonthlyFuturesFixings(Trafigura.Bloomberg.Futures.Comex, FuturesExchangeFactory.COMEX.fixingLevel)) ::: SpotFXFixings.all)
}

case class PriceFixingLimMarketDataSource(service: LIMService, emailService: EmailService, template: Email)
  extends LimMarketDataSource(service) {

  import PriceFixingLimMarketDataSource._

  override def description = List(fixingsSources).flatMap
    { case (marketDataType, sources) => marketDataType.name.pair(sources.flatMap(_.description)).map("%s → %s" % _) }

  def read(day: Day) = log.infoWithTime("Getting data from LIM") {
    Map(getValuesForType(PriceFixingsHistoryDataType.name, day.startOfFinancialYear, day, fixingsSources))
  }

  override def eventSources(marketDataStore: MarketDataStore) = {
    List(new PriceFixingDataEventSource(PricingGroup.Metals, ReferenceInterestMarketDataProvider(marketDataStore)))
  }

  override def availabilityTasks(marketDataStore: MarketDataStore) = List(
    TaskDescription("Verify SIBOR Available", daily(LME.calendar, 4 H 30), notImplemented),
    TaskDescription("Verify ROBOR Available", daily(LME.calendar, 9 H 00), notImplemented),
    TaskDescription("Verify JIBAR Available", daily(LME.calendar, 10 H 30), notImplemented),
    TaskDescription("Verify Libor maturities available", daily(LME.calendar, 12 H 00),
      new VerifyLiborMaturitiesAvailable(marketDataStore, emailService, template).withSource("LIM")),
    TaskDescription("Verify LME AMR1 Fixings Available", daily(LME.calendar, 12 H 30), notImplemented),
  //      registerTasks(tasks(daily(LME, 13 H 15), TRAF.LME.{EUR, GBP, JPY} SpotFX
    TaskDescription("Verify LME OFFICIAL Fixings Available", daily(LME.calendar, 13 H 30), notImplemented),
    TaskDescription("Verify LME PMR1 LMEFixings Available", daily(LME.calendar, 16 H 30), notImplemented),
    TaskDescription("Verify LME UNOFFICIAL LMEFixings Available", daily(LME.calendar, 17 H 00), notImplemented),
    TaskDescription("Verify IRS Available", daily(LME.calendar, 18 H 00), notImplemented),
    TaskDescription("Verify BloombergTokyoCompositeFXRates Available", daily(LME.calendar, 20 H 00), notImplemented),
    TaskDescription("Verify BalticFixings Available", daily(LME.calendar, 20 H 00), notImplemented),

    TaskDescription("Verify SHIBOR Available", daily(NYMEX.calendar, 0 H 00), notImplemented),
    TaskDescription("Verify MIBOR-1 Available", daily(NYMEX.calendar, 1 H 00), notImplemented),
    TaskDescription("Verify AIDIBOR Available", daily(NYMEX.calendar, 3 H 00), notImplemented),
    TaskDescription("Verify MIBOR-2 Available", daily(NYMEX.calendar, 5 H 00), notImplemented),
    TaskDescription("Verify BUBOR Available", daily(NYMEX.calendar, 05 H 00), notImplemented),
    TaskDescription("Verify SOFIBOR Available", daily(NYMEX.calendar, 05 H 00), notImplemented),
    TaskDescription("Verify TRLIBOR Available", daily(NYMEX.calendar, 6 H 00), notImplemented),
    TaskDescription("Verify NIBOR Available", daily(NYMEX.calendar, 7 H 00), notImplemented),
    TaskDescription("Verify COMEX Fixings Available", daily(COMEX.calendar, 20 H 00), notImplemented),
    TaskDescription("Verify KLIBOR Available", daily(NYMEX.calendar, 23 H 00), notImplemented),

    TaskDescription("Verify HIBOR Available", daily(SFS.calendar, 11 H 00), notImplemented),
    TaskDescription("Verify Shfe Fixings Available", daily(SFS.calendar, 20 H 00), notImplemented)
  ).filterNot(_.task == notImplemented)
}

object LMEFixings extends LimSource(List(Ask, Bid)) {
  type Relation = LMEFixingRelation
  def description = List("%s/TRAF.LME.<commodity>.<ring>.<tenor> %s" % (Trafigura.Bloomberg.Currencies.Lme, levelDescription))

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

class MonthlyFuturesFixings(parentNodes: List[LimNode], levels: List[Level]) extends HierarchicalLimSource(parentNodes, levels) {
  def this(node: LimNode, level: Level) = this(List(node), List(level))

  type Relation = MonthlyFuturesRelation

  case class MonthlyFuturesRelation(market: FuturesMarket, month: Month)

  def relationExtractor = Extractor.regex("""TRAF\.(\w+)\.(\w+)_(\w+)""") { case List(exchange, limSymbol, deliveryMonth) => {
    val optMarket = Market.fromExchangeAndLimSymbol(exchangeLookup(exchange), limSymbol)
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

class VerifyLiborMaturitiesAvailable(marketDataStore: MarketDataStore, emailService: EmailService, template: Email)
  extends EmailingScheduledTask(emailService, template) {

  import LIBORFixing._

  protected def emailFor(observationDay: Day) = {
    val liborFixings: NestedMap[UOM, Tenor, (Percentage, Day)] = latestLiborFixings(marketDataStore, observationDay)
    val tenorsByCurrency = liborFixings.mapValues(_.keys.toList).withDefaultValue(Nil)
    val missingTenorsByCurrency = currencies.toMapWithValues(currency => tenorsFor(currency) \\ tenorsByCurrency(currency))
      .filterValuesNot(_.isEmpty).sortBy(_.toString)

    (missingTenorsByCurrency.size > 0).option {
      template.copy(subject = "Missing Libor Maturities in LIM, observation day: " + observationDay,
        body = <span>
                 <p>The following LIBOR tenors are required by Trinity but are missing in LIM</p>
                 <table>
                   { for ((currency, missingTenors) <- missingTenorsByCurrency) yield
                     <tr><td><b>{currency}: </b></td><td>{missingTenors.mkString(", ")}</td></tr>
                   }
                 </table>
               </span>.toString)
    }
  }

  def latestLiborFixings(marketDataStore: MarketDataStore, observationDay: Day): NestedMap[UOM, Tenor, (Percentage, Day)] = {
    liborFixingsHistoryData(marketDataStore, observationDay).mapValues(_.fixingsFor(periods).collect {
      case ((Level.Close, StoredFixingPeriod.Tenor(tenor)), MarketValue.Percentage(percentage)) => {
        tenor → (percentage, observationDay)
      }
    })
  }

  private def liborFixingsHistoryData(marketDataStore: MarketDataStore, observationDay: Day): Map[UOM, PriceFixingsHistoryData] =
    currencies.toMapWithSomeValues(currency => safely(read(marketDataStore, observationDay, currency)).toOption)

  private def read(marketDataStore: MarketDataStore, observationDay: Day, currency: UOM) =
    latestLimOnlyMarketDataReader(marketDataStore).readAs[PriceFixingsHistoryData](TimedMarketDataKey(
      observationDay.atTimeOfDay(ObservationTimeOfDay.LiborClose), PriceFixingsHistoryDataKey(currency.toString, Some("LIBOR"))))

  private def latestLimOnlyMarketDataReader(marketDataStore: MarketDataStore) = new NormalMarketDataReader(
    marketDataStore, marketDataStore.latestMarketDataIdentifier(MarketDataSelection(Some(PricingGroup.Metals))))
}