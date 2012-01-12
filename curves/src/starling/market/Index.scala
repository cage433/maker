package starling.market

import formula.FormulaIndex
import rules._
import starling.curves._
import starling.daterange._
import starling.marketdata.PriceFixingsHistoryDataKey
import starling.calendar.BusinessCalendar
import starling.utils.ImplicitConversions._
import starling.utils.Pattern.Extractor
import starling.quantity._
import java.lang.IllegalStateException
import starling.pivot.MarketValue

case class UnknownIndexException(msg: String, eaiQuoteID: Option[Int] = None) extends Exception(msg)

trait Index extends KnownConversions {
  val name : String
  val eaiQuoteID: Option[Int] = None

  def identifier = name
  def priceUOM : UOM
  def uom : UOM

  def precision: Option[Precision]

  def calendars : Set[BusinessCalendar]

  override def toString = name

  /**
   * Hack so I can change how indices are displayed in pvot reports.This should be replaced with 'name',
   * and the strings used in Starling DB changed, however I don't know if this will mess up reading
   * from Trinity. TODO [26 Nov 2010] findout
   */
  def reportDisplayName = name

  def convert(value: Quantity, uom: UOM): Option[Quantity]

  override def equals(obj: Any): Boolean = obj match {
    case other: Index => name.equalsIgnoreCase(other.name)
    case _ => false
  }

  override lazy val hashCode = name.hashCode

  /**
   * Swaps and asians often have start/end date not on month boundaries in EAI. In general they do mean the whole month
   * as missing days are not observation days
   */
  def makeAveragingPeriodMonthIfPossible(dateRange : DateRange, rule: SwapPricingRule) : DateRange = {
    if (dateRange.isInstanceOf[Month])
      dateRange
    else {
      val month: Month = dateRange.firstMonth
      val calendar = rule.calendar(calendars)
      if ((month == dateRange.lastMonth) && (month.days.filter(calendar.isBusinessDay) == dateRange.days.filter(calendar.isBusinessDay))){
        month
      } else {
        dateRange
      }
    }
  }

  def commodity : Commodity

  def possiblePricingRules: List[SwapPricingRule]

  def indexes: Set[SingleIndex]
}

case class IndexSensitivity(coefficient : Double, index : Index)

/**
 * The canonical example is the Ave 4 LME index. It's not a single index as it depends on both
 * cash and 3 month prices, but it does have a single price for each day, which can be displayed in the UI
 * explanation of the valuation of physical forwards
 */
trait IndexWithDailyPrices extends Index with KnownObservation {
  final def fixingOrForwardPrice(env : Environment, observationDay : Day) : Quantity = {
    val price = provideFixingOrForwardPrice(env, observationDay)

    price.ensuring(_.uom == priceUOM, "%s.fixingOrForwardPrice(%s, %s) = %s does not have correct uom: %s" %
      (name, env, observationDay,  price, priceUOM))

    price
  }

  def market : CommodityMarket
  def lotSize = market.lotSize

  def businessCalendar = market.businessCalendar
  def precision = market.precision
  def commodity = market.commodity
  def priceUOM : UOM = market.priceUOM
  def currency = market.currency
  def uom = market.uom
  def calendars = Set(businessCalendar)
  def convert(value: Quantity, uom: UOM): Option[Quantity] = market.convert(value, uom)
  def possiblePricingRules = List(NoPricingRule)
  def isObservationDay(day: Day): Boolean = businessCalendar.isBusinessDay(day)

  protected def provideFixingOrForwardPrice(env : Environment, observationDay : Day) : Quantity

  // base implementation of average price for index, may be overridden in specific indices
  def averagePrice(averagingPeriod: DateRange, rounding: Option[Int], env : Environment): Quantity = {
    val observationDays = this.observationDays(averagingPeriod)
    val prices = observationDays.map(fixingOrForwardPrice(env, _))

    val price = Quantity.average(prices) match {
      case nq : NamedQuantity => SimpleNamedQuantity("Average(" + this + "." + averagingPeriod + ")", nq)
      case q : Quantity => q
    }
    rounding match {
      case Some(dp) if env.environmentParameters.swapRoundingOK => {
        price.round(dp)
      }
      case _ => price
    }
  }
  }

/**
 * An index on a single underlying
 */
trait SingleIndex extends IndexWithDailyPrices with FixingHistoryLookup {
  override val name: String

  def level : Level

  def fixing(env : InstrumentLevelEnvironment, observationDay : Day) = {
    env.quantity(IndexFixingKey(this, observationDay)) match {
      case nq : NamedQuantity => {
        val fixed = new SimpleNamedQuantity(observedPeriod(observationDay).toShortString + " Fixed", new Quantity(nq.value, nq.uom))
        SimpleNamedQuantity(observationDay.toString, fixed)
      }
      case q => q
    }
  }

  def forwardPriceOnObservationDay(env: InstrumentLevelEnvironment, observationDay: Day, ignoreShiftsIfPermitted: Boolean) = {
    env.quantity(ForwardPriceKey(market, observedPeriod(observationDay), ignoreShiftsIfPermitted)) match {
      case nq : NamedQuantity =>  SimpleNamedQuantity(observationDay.toString, nq.withNewName(observedPeriod(observationDay).toShortString))
      case q => q
    }
  }

  protected def provideFixingOrForwardPrice(env : Environment, observationDay : Day) = env.fixingOrForwardPrice(this, observationDay)

  def observationTimeOfDay = ObservationTimeOfDay.Default

  def storedFixingPeriod(date: Either[Day, DateRange]): StoredFixingPeriod = date match {
    case Left(day) => storedFixingPeriodForDay(day)
    case _ => throw new IllegalArgumentException("Invalid param for index: " + date)
  }

  def storedFixingPeriodForDay(day:Day) : StoredFixingPeriod

  def fixing(slice: MarketDataSlice, observationDay : Day, storedFixingPeriod: Option[StoredFixingPeriod]): Quantity = {
    require(storedFixingPeriod.isEmpty)
    val key = PriceFixingsHistoryDataKey(market)
    val fixingHistory = slice.fixings(key, ObservationPoint(observationDay, observationTimeOfDay))
    val fixing: MarketValue = try {
      fixingHistory.fixingFor(level, storedFixingPeriodForDay(observationDay))
    } catch {
      case ex => throw new MissingMarketDataException("Index " + this + ", observation day " + observationDay + ", period " + storedFixingPeriod + ", " + ex.getMessage, ex)
    }
    fixing.toQuantity
  }

   /*
     Shift vols of prices for the underlying market/period so that swap vols are also perturbed. Needs to be used with care,
     as it is possible that the SwapVol differentiable concerned may not be the only thing perturbed. Other SwapVols may depend
     on the same underlying prices. Compare with perturbations in InstrumentLevelEnvironment where vols for an exact averaging period
     are perturbed.
   */
  def shiftedUnderlyingVols(env : Environment, averagingPeriod : DateRange, dP : Quantity) = {
    val averagingDays : Seq[Day] = averagingPeriod.days.filter(isObservationDay)
    val firstObservedDay : Day = averagingDays.map{d => observedPeriod(d).firstDay}.sortWith(_<_).head
    val lastObservedDay : Day = averagingDays.map{d => observedPeriod(d).lastDay}.sortWith(_>_).head
    val observedPeriodsUnion = DateRange(firstObservedDay, lastObservedDay)
    val dV = dP.toPercentage
    val upEnv = env.shiftVol(market, Some(averagingPeriod), observedPeriodsUnion, dV)
    val downEnv = env.shiftVol(market, Some(averagingPeriod), observedPeriodsUnion, -dV)
    (downEnv, upEnv)
  }

  /**
   * Kind of sucks having to pass in average price. It's the forward price, not necessarily for the observed
   * period for this day, rather the observed period for the averaging period for which the vol is being
   * calculated.
   */
  def volatility(env : InstrumentLevelEnvironment, observationDay : Day, strike : Quantity, averagePrice : Quantity) = {
    def expiry(period : DateRange) = market match {
      case k: KnownExpiry => k.optionExpiry(period)
      case _ => period.firstDay // HACk
    }
    env.interpolatedVol(market, observedOptionPeriod(observationDay), Some(observationDay), Some(strike), isIndexVol = true, Some(averagePrice))
  }

  def makeAveragingPeriodMonthIfPossible(dateRange: DateRange): DateRange = {
    makeAveragingPeriodMonthIfPossible(dateRange, CommonPricingRule)
  }

  def observedPeriod(observationDay : Day) : DateRange

  // There should really be a different index for this
  def observedOptionPeriod(observationDay: Day) : DateRange

  def indexes = Set(this)
  }

case class PublishedIndex(
  override val name: String,
  override val eaiQuoteID: Option[Int],
  override val lotSize: Option[Double],
  override val uom: UOM,
  override val currency: UOM,
  override val businessCalendar: BusinessCalendar,
  override val commodity: Commodity,
  override val conversions: Conversions = Conversions.default,
  override val limSymbol: Option[LimSymbol] = None,
  override val precision : Option[Precision] = None,
  override val level: Level = Level.Mid) extends CommodityMarket(name, lotSize, uom, currency, businessCalendar, eaiQuoteID, Day, commodity, conversions, limSymbol, precision) with SingleIndex
{
  type marketType = CommodityMarket

  override def isObservationDay(day: Day): Boolean = super[SingleIndex].isObservationDay(day)

  override def convert(value: Quantity, uom: UOM): Option[Quantity] = {
     value.in(uom)(conversions)
   }

//  override def convert(value: Quantity, uom: UOM) = super[SwapMarket].convert(value, uom)

//  override def convertUOM(value: Quantity, uom: UOM) : Quantity = super[SwapMarket].convertUOM(value, uom)
  override def convertUOM(volume : Quantity, uom : UOM) : Quantity = {
    convert(volume, uom) match {
      case Some(beqv) => beqv
      case None => throw new Exception(this + ": Couldn't convert from " + volume + " to " + uom)
    }
  }

  def observedPeriod(observationDay : Day) : DateRange = observationDay

  def storedFixingPeriodForDay(observationDay: Day) = StoredFixingPeriod.dateRange(observedPeriod(observationDay))

  def observedOptionPeriod(observationDay: Day) = observationDay

  override def priceUOM =  super[CommodityMarket].priceUOM

  def market: CommodityMarket = this

  override def fixing(slice: MarketDataSlice, observationDay: Day, storedFixingPeriod: Option[StoredFixingPeriod]) = super[SingleIndex].fixing(slice, observationDay, storedFixingPeriod)

  override def fixing(env: InstrumentLevelEnvironment, observationDay: Day) = super[SingleIndex].fixing(env, observationDay)

  override def priceFromForwardPrices(prices: Map[DateRange, Quantity], dateRange: DateRange) = dateRange match {
    case day: Day => {
      val orderedDays = prices.keySet.asInstanceOf[Set[Day]].toList.sortWith(_ < _).toArray
      val orderedPrices = orderedDays.map(prices).toArray

      InverseConstantInterpolation.interpolate(orderedDays, orderedPrices, day)
    }
    case _ => throw new Exception("Market " + this + " is daily so does not directly support prices for " + dateRange)
  }
}

case class FuturesFrontPeriodIndex(
  marketName: String,
  override val eaiQuoteID: Option[Int],
  override val market : FuturesMarket,
  rollBeforeDays : Int,
  promptness : Int,
  override val precision: Option[Precision]
 ) extends SingleIndex {

  def futuresMarket = market
  val name = marketName
  lazy val level = market.exchange.fixingLevel

  def observedPeriod(observationDay : Day) : DateRange = {
    val frontMonth = market.frontPeriod(observationDay.addBusinessDays(market.businessCalendar, rollBeforeDays))
    val period = frontMonth match {
      case month:Month => month + (promptness - 1)
      case other => other //This happens in a test but may not be needed when run with real data
    }
    period
  }
  def storedFixingPeriodForDay(observationDay: Day) = StoredFixingPeriod.dateRange(observedPeriod(observationDay))

  def observedOptionPeriod(observationDay: Day) = market.frontOptionPeriod(
    observationDay.addBusinessDays(market.businessCalendar, rollBeforeDays)
  )

  def frontFuturesMonthToSwapMonth(frontFuturesMonth : Month) : Month = {
    market.lastTradingDay(frontFuturesMonth).containingMonth
  }

  override def reportDisplayName = "%s 1st %s" % (market.name, market.tenor.toString.toLowerCase)

  /**
   * This works for Comex and SFSE - may need to be overriden for others.
   */
  override def observationTimeOfDay = market.exchange.closeTime

  /**
   * The date range when a particular contract is the front 
   */
  def frontContractPeriod(month : Month) = market.lastTradingDay(month - 1).nextBusinessDay(businessCalendar) upto market.lastTradingDay(month)
}

abstract class MultiIndex(override val name: String) extends Index {
  /**
   * To avoid blank rows cash needs to be associated with a risk market and period. 
   * Doesn't really amtter which we use - just making sure the choice is consistent
   */
  lazy val arbitraryIndexToAssignCashTo = indexes.toList.sortWith(_.toString < _.toString).head

  def averagePrice(env : Environment, averagingPeriod: DateRange, pricingRule: SwapPricingRule, priceUOM: UOM,
                   rounding: Option[Int], roundingMethodRule: RoundingMethodRule): Quantity

  def calendars = indexes.flatMap(_.calendars)

  def commodity = {
    val commodities = indexes.map(_.commodity)
    if (commodities.size == 1)
      commodities.head
    else
      throw new Exception("Not exactly one commodity")
  }

  def convert(value: Quantity, uom: UOM): Option[Quantity] = {
    val conversions = indexes.map(_.convert(value, uom))
    if(conversions.size != 1)
      None
    else
      conversions.head
  }

  protected def checkedConvert(index: Index, price: Quantity, priceUOM: UOM): Quantity = index.convert(price, priceUOM) match {
    case Some(p) => p
    case None => throw new Exception(this + ": Couldn't convert from " + price.uom + " to " + priceUOM + " with " + index)
  }

  def possiblePricingRules = List(CommonPricingRule, NonCommonPricingRule)
}

object Index {
  private def provider = MarketProvider.provider

  def indexFromName(name: String): Index = provider.index(name).getOrElse(throw new Exception("No index: " + name))
  def publishedIndexFromName(name: String): PublishedIndex = indexFromName(name).cast[PublishedIndex]
  def futuresFrontPeriodIndexFromName(name: String): FuturesFrontPeriodIndex = indexFromName(name).asInstanceOf[FuturesFrontPeriodIndex]
  def formulaIndexFromName(name: String): FormulaIndex = indexFromName(name).asInstanceOf[FormulaIndex]

  val FromName = Extractor.from[String](provider.index)
  val PublishedIndex = FromName andThen(_.safeCast[PublishedIndex])

  lazy val WTI10 = futuresFrontPeriodIndexFromName("NYMEX WTI 1st month")
  lazy val WTI20 = futuresFrontPeriodIndexFromName("NYMEX WTI 2nd month")
  lazy val ICEWTI10 = futuresFrontPeriodIndexFromName("ICE WTI 1st month")

  lazy val BRT11 = futuresFrontPeriodIndexFromName("IPE Brent 1st month")
  lazy val GO11  = futuresFrontPeriodIndexFromName("IPE Gas Oil 1st month (Settlement)")
  lazy val HO10  = futuresFrontPeriodIndexFromName("NYMEX Heat 1st month")
  lazy val RBOB10  = futuresFrontPeriodIndexFromName("NYMEX Rbob 1st month")
  lazy val NYMGO11 = futuresFrontPeriodIndexFromName("NYMEX Gas Oil 1st month")

  lazy val PLATTS_BRENT_1ST_MONTH = publishedIndexFromName("Platts Brent 1st month")
  lazy val DATED_BRENT = publishedIndexFromName("Dated Brent")
  lazy val PREM_UNL_EURO_BOB_OXY_NWE_BARGES = publishedIndexFromName("Prem Unl Euro-Bob Non-Oxy NWE Barges (Argus)")
  lazy val UNL_87_USGC_PIPELINE = publishedIndexFromName("Unl 87 USGC Pipeline")
  lazy val MOGAS_95_UNL_10PPM_NWE_BARGES = publishedIndexFromName("Mogas 95 Unl 10ppm NWE Barges (Argus)")
  lazy val CAPSIZE_TC_AVG = publishedIndexFromName("Capesize T/C Average (Baltic)")

  lazy val IPE_GAS_OIL_VS_IPE_BRENT = formulaIndexFromName("IPE Gas Oil (Settlement) vs IPE Brent")
  lazy val MOGAS_95_UNL_10PPM_NWE_BARGES_VS_IPE_BRENT = formulaIndexFromName("Mogas 95 Unl 10ppm NWE Barges (Argus) vs IPE Brent")
  lazy val NYMEX_WTI_VS_IPE_BRENT = formulaIndexFromName("NYMEX WTI vs IPE Brent")
  lazy val NYMEX_RBOB_1ST_MONTH_VS_IPE_BRENT_1ST_MONTH = formulaIndexFromName("nymex rbob 1st month vs ipe brent 1st month (bbls)")
  lazy val NYMEX_HEAT_1ST_MONTH_VS_ICE_BRENT_1ST_MONTH = formulaIndexFromName("NYMEX Heat 1st Month vs ICE Brent 1st Month")

  lazy val TC6_CROSS_MEDITERRANEAN_30KT_BALTIC = publishedIndexFromName("TC6 Cross Mediterranean 30KT (Baltic)")
  lazy val NAPHTHA_CFR_JAPAN = publishedIndexFromName("Naphtha CFR Japan")

  def futuresMarketToIndex(market: FuturesMarket): Option[FuturesFrontPeriodIndex] = provider.futuresMarketToIndex(market)

  /**
   * Views of current set of indexes
   */
  def lmeIndicesView = {
    for (
      market <- Market.futuresMarketsView;
      if (market.exchange == FuturesExchangeFactory.LME);
      level <- List(Level.Bid, Level.Ask)
    )
    yield (LmeCashSettlementIndex(market, level))
  }
  def allView : List[Index] = provider.allIndexes ::: lmeIndicesView ::: BrentCFDSpreadIndex.all
  def futuresMarketIndexesView : List[FuturesFrontPeriodIndex] = allView.flatMap{case i:FuturesFrontPeriodIndex => Some(i); case _ => None}
  def publishedIndexesView : List[PublishedIndex] = allView.flatMap{case i:PublishedIndex => Some(i); case _ => None}
  def formulaIndexesView : List[FormulaIndex] = allView.flatMap{case i:FormulaIndex => Some(i); case _ => None}
  def singleIndexesView = allView.flatMap{
    case si: SingleIndex => Some(si)
    case _ => None
  }

  def fromNameOption(name : String) = allView.find(_.name == name)

  def fromName(name : String) = fromNameOption(name) match {
    case Some(index) => index
    case None => throw new UnknownIndexException("No index with name " + name + " in " + allView)
  }

  def singleIndexFromName(name: String) = fromName(name) match {
    case si: SingleIndex => si
    case other => throw new Exception(other + " is not of type TrinityIndex")
  }

  def indexFromEAIQuoteID(id: Int): Index = provider.index(id) match {
    case Some(i:FormulaIndex) => i.verify
    case Some(i) => i
    case None => {
      throw new UnknownIndexException(id + " is not a known Index eaiQuoteID", Some(id))
    }
  }

  def indexOptionFromEAIQuoteID(id: Int) = provider.index(id)

  def unapply(eaiQuoteID: Int) = indexOptionFromEAIQuoteID(eaiQuoteID)

  def singleIndexFromEAIQuoteID(id: Int): SingleIndex = provider.index(id) match {
    case Some(i:SingleIndex) => i
    case Some(i) => throw new UnknownIndexException(id + " is not a SingleIndex: " + i, Some(id))
    case None => throw new UnknownIndexException(id + " is not a known Index eaiQuoteID", Some(id))
  }

  def markets(index: Index): Set[CommodityMarket] = index.indexes.flatMap {
    case si => Set(si.market.asInstanceOf[CommodityMarket])
  }

  def getPublishedIndexForMarket(market: CommodityMarket): Option[PublishedIndex] = provider.getPublishedIndexForMarket(market)
}

object LmeSingleIndices{
  val cuCashBid = new LmeCashSettlementIndex(Market.LME_COPPER, level = Level.Bid)
  val cuCashOffer = new LmeCashSettlementIndex(Market.LME_COPPER, level = Level.Ask)
  val cu3MBid = new LmeThreeMonthIndex(Market.LME_COPPER, level = Level.Bid)
  val cu3MOffer = new LmeThreeMonthIndex(Market.LME_COPPER, level = Level.Ask)

  val alCashBid = new LmeCashSettlementIndex(Market.LME_ALUMINIUM, level = Level.Bid)
  val alCashOffer = new LmeCashSettlementIndex(Market.LME_ALUMINIUM, level = Level.Ask)
  val al3MBid = new LmeThreeMonthIndex(Market.LME_ALUMINIUM, level = Level.Bid)
  val al3MOffer = new LmeThreeMonthIndex(Market.LME_ALUMINIUM, level = Level.Ask)

  val niCashBid = new LmeCashSettlementIndex(Market.LME_NICKEL, level = Level.Bid)

  val niCashOffer = new LmeCashSettlementIndex(Market.LME_NICKEL, level = Level.Ask)
  val ni3MBid = new LmeThreeMonthIndex(Market.LME_NICKEL, level = Level.Bid)
  val ni3MOffer = new LmeThreeMonthIndex(Market.LME_NICKEL, level = Level.Ask)
}

object ShfeIndices {
  def shfeMonthVwap(market : FuturesMarket) = ShfeVwapMonthIndex(market)
  val shfeAlMonthVwap = ShfeVwapMonthIndex(Market.SHANGHAI_ALUMINUIUM)
}

object FuturesFrontPeriodIndex {
  def apply(market : FuturesMarket, promptness : Int = 1) = {
    val name = "%s 1st %s" % (market.name, market.tenor.toString.toLowerCase)
    new FuturesFrontPeriodIndex(name, None, market, 0, promptness, None)
  }
}

trait LMESingleIndex extends SingleIndex {
  def observedOptionPeriod(observationDay: Day) = throw new Exception("Options not supported for LME indices")
  override def observationTimeOfDay = ObservationTimeOfDay.LME_Official
}
case class LmeCashSettlementIndex(market : FuturesMarket, level : Level) extends LMESingleIndex {
  val name = "LME " + market.commodity + " cash " + level.name

  def observedPeriod(day : Day) = {
    assert(isObservationDay(day), day + " is not an observation day for " + this)
    day.addBusinessDays(businessCalendar, 2)
  }

  def storedFixingPeriodForDay(day: Day) = StoredFixingPeriod.tenor(Tenor.CASH)

  override def observationTimeOfDay = ObservationTimeOfDay.LME_Official
}

case class LmeThreeMonthIndex(market : FuturesMarket, level : Level) extends LMESingleIndex{
  val name = "LME " + market.commodity + " 3m " + level.name

  def observedPeriod(day : Day) = {
    assert(isObservationDay(day), day + " is not an observation day for " + this)
    FuturesExchangeFactory.LME.threeMonthDate(day)
  }

  def storedFixingPeriodForDay(day: Day) = StoredFixingPeriod.tenor(Tenor.ThreeMonths)
}

case class LmeLowestOfFourIndex(market : FuturesMarket) extends IndexWithDailyPrices {
  private val cashIndex = LmeCashSettlementIndex(market, Level.Bid)
  private val threeMonthIndex = LmeThreeMonthIndex(market, Level.Bid)

  //override def level(observationDay : Day) = Level.Bid

  protected def provideFixingOrForwardPrice(env : Environment, observationDay : Day) : Quantity = {
    cashIndex.fixingOrForwardPrice(env, observationDay) min threeMonthIndex.fixingOrForwardPrice(env, observationDay)
  }
  def indexes = Set(cashIndex, threeMonthIndex)
  val name = "LME " + market.commodity + " low 4"
}

case class LmeAverageOfFourIndex(market : FuturesMarket) extends IndexWithDailyPrices {
  private val cashBidIndex = LmeCashSettlementIndex(market, Level.Bid)
  private val cashAskIndex = LmeCashSettlementIndex(market, Level.Ask)
  private val threeMonthBidIndex = LmeThreeMonthIndex(market, Level.Bid)
  private val threeMonthAskIndex = LmeThreeMonthIndex(market, Level.Ask)

  def provideFixingOrForwardPrice(env : Environment, observationDay : Day) : Quantity = {
    Quantity.average(
      indexes.toList.map(_.fixingOrForwardPrice(env, observationDay))
    )
  }
  def indexes = Set(cashBidIndex, cashAskIndex, threeMonthBidIndex, threeMonthAskIndex)
  val name = "LME " + market.commodity + " average 4"
}

case class LmeAve4MaxSettIndex(market : FuturesMarket) extends IndexWithDailyPrices {
  private val ave4Index = LmeAverageOfFourIndex(market)
  private val cashAskIndex = LmeCashSettlementIndex(market, Level.Ask)
  def indexes = ave4Index.indexes ++ cashAskIndex.indexes
  def provideFixingOrForwardPrice(env : Environment, observationDay : Day) : Quantity = {
    // Despite the misleading name for this index, it is actually a minimum
    ave4Index.fixingOrForwardPrice(env, observationDay) min cashAskIndex.fixingOrForwardPrice(env, observationDay)
  }

  val name = "LME " + market.commodity + " ave 4 max sett"
}

/**
 * special type of index using unweighted arithmetic average prices,
 *   until official published monthly wwap prices are available and then using the vwap fixing instead
 */
object ShfeVwapMonthIndex {
  def apply(market : FuturesMarket) = {
    val name = "%s 1st %s" % (market.name, market.tenor.toString.toLowerCase)
    new ShfeVwapMonthIndex(name, market)
  }
}

/**
 * Wraps a normal front futures market with handling for end of month official month vwap price fixings
 */
case class ShfeVwapMonthIndex(marketName : String, market : FuturesMarket) extends SingleIndex {

  def level = Level.VwapMonthIndexLevel

  // underlying index for "published" price fixings
  val baseFrontFuturesMarketIndex = FuturesFrontPeriodIndex(market)

  // this rule may need changing but is literally the last business day (eod) of the markets business calendar by default for now
  private def publicationDay(averagingPeriod : DateRange) = averagingPeriod.lastDay.thisOrPreviousBusinessDay(market.businessCalendar).endOfDay

  // switch between published monthly vwap and official daily (unweighted) prices according to the environment's market day
  override def averagePrice(averagingPeriod: DateRange, rounding: Option[Int], env : Environment): Quantity = {

    val pubDay = publicationDay(averagingPeriod)
    if (env.marketDay >= pubDay)
      env.indexFixing(this, pubDay.day)
    else
      baseFrontFuturesMarketIndex.averagePrice(averagingPeriod, rounding, env)
  }

  def observedOptionPeriod(observationDay: Day) = baseFrontFuturesMarketIndex.observedOptionPeriod(observationDay)

  def observedPeriod(day : Day) = baseFrontFuturesMarketIndex.observedPeriod(day)

  def storedFixingPeriodForDay(day: Day) = baseFrontFuturesMarketIndex.storedFixingPeriodForDay(day)

  val name = marketName +  " Month VWAP"
}
