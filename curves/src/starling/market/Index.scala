package starling.market

import formula.FormulaIndex
import rules._
import starling.utils.CaseInsensitive
import starling.utils.ImplicitConversions._
import starling.curves._
import starling.daterange._
import starling.marketdata.PriceFixingsHistoryDataKey
import starling.calendar.{BusinessCalendar, BrentMonth}
import starling.utils.cache.CacheFactory
import starling.quantity.{Percentage, Quantity, UOM}


case class UnknownIndexException(msg: String, eaiQuoteID: Option[Int] = None) extends Exception(msg)

abstract class Index(val name : CaseInsensitive) {
  val identifier = name.toString
  def priceUOM : UOM
  def uom : UOM

  def precision: Option[Precision]

  def markets: List[CommodityMarket]

  override def toString = name

  /**
   * Hack so I can change how indices are displayed in pvot reports.This should be replaced with 'name',
   * and the strings used in Starling DB changed, however I don't know if this will mess up reading
   * from Trinity. TODO [26 Nov 2010] findout
   */
  def reportDisplayName = name

  def convert(value: Quantity, uom: UOM): Option[Quantity]

  def convertUOM(value: Quantity, uom : UOM) : Quantity = {
    convert(value, uom) match {
      case Some(beqv) => beqv
      case None => throw new Exception(this + ": Couldn't convert from " + value + " to " + uom)
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: Index => name == other.name
    case _ => false
  }

  override def hashCode = name.hashCode

  /**
   * Swaps and asians often have start/end date not on month boundaries in EAI. In general they do mean the whole month
   * as missing days are not observation days
   */
  def makeAveragingPeriodMonthIfPossible(dateRange : DateRange, rule: SwapPricingRule) : DateRange = {
    if (dateRange.isInstanceOf[Month])
      dateRange
    else {
      val month: Month = dateRange.firstMonth
      if ((month == dateRange.lastMonth) && (month.days.filter(rule.isObservationDay(markets, _)) == dateRange.days.filter(rule.isObservationDay(markets, _)))){
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
 * An index whose forward prices come from a single market. It is possible that many indices, e.g. LME fixings
 * for different rings, will all have the same forward prices
 */
abstract class SingleIndex(val forwardPriceMarket : CommodityMarket, name : String) extends Index(name){
  @transient protected lazy val observationDayCache = CacheFactory.getCache(name, unique = true)
  def observationDays(period : DateRange) : List[Day] = observationDayCache.memoize(period, period.days.filter(isObservationDay).toList)

  def fixing(slice : MarketDataSlice, observationDay : Day) : Quantity
  def fixing(env : InstrumentLevelEnvironment, observationDay : Day) : Quantity
  def forwardPrice(env : InstrumentLevelEnvironment, observationDay : Day, ignoreShiftsIfPermitted : Boolean) : Quantity
  def businessCalendar : BusinessCalendar
  def isObservationDay(day: Day): Boolean = businessCalendar.isBusinessDay(day)
  /**
   * Kind of sucks having to pass in average price. It's the forward price, not necessarily for the observed
   * period for this day, rather the observed period for the averaging period for which the vol is being
   * calculated.
   */
  def volatility(env : InstrumentLevelEnvironment, observationDay : Day, strike : Quantity, averagePrice : Quantity) : Percentage
  def shiftedUnderlyingVols(env : Environment, averagingPeriod : DateRange, dP : Quantity)  : (Environment, Environment)
  def priceUOM : UOM = forwardPriceMarket.priceUOM
  def currency = forwardPriceMarket.currency
  def uom = forwardPriceMarket.uom

  def lotSize : Option[Double]          // Some trades are stored in EAI with number of lots instead of volume

  def makeAveragingPeriodMonthIfPossible(dateRange: DateRange): DateRange = {
    makeAveragingPeriodMonthIfPossible(dateRange, CommonPricingRule)
  }

  def observedPeriod(observationDay : Day) : DateRange// = fixingPeriod(observationDay).period(observationDay)

  // There should really be a different index for this
  def observedOptionPeriod(observationDay: Day) : DateRange


}
/**
 * An Index with a single underlying market
 */
abstract class SimpleSingleIndex(name: String, forwardPriceMarket : CommodityMarket, val lotSizeOverride: Option[Double] = None,
                           val level: Level = Level.Unknown) extends SingleIndex(forwardPriceMarket, name) {


  def precision = forwardPriceMarket.precision

  def commodity = forwardPriceMarket.commodity

  def markets = List(forwardPriceMarket)

  def observationTimeOfDay = ObservationTimeOfDay.Default

  def lotSize = lotSizeOverride match {
    case Some(ls) => Some(ls)
    case None => forwardPriceMarket.lotSize
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
    val dV = Percentage(dP.checkedValue(UOM.SCALAR))
    val upEnv = env.shiftVol(forwardPriceMarket, Some(averagingPeriod), observedPeriodsUnion, dV)
    val downEnv = env.shiftVol(forwardPriceMarket, Some(averagingPeriod), observedPeriodsUnion, -dV)
    (downEnv, upEnv)
  }

  /**
   * Kind of sucks having to pass in average price. It's the forward price, not necessarily for the observed
   * period for this day, rather the observed period for the averaging period for which the vol is being
   * calculated.
   */
  def volatility(env : InstrumentLevelEnvironment, observationDay : Day, strike : Quantity, averagePrice : Quantity) = {
    def expiry(period : DateRange) = forwardPriceMarket match {
      case k: KnownExpiry => k.optionExpiry(period)
      case _ => period.firstDay // HACk
    }
    env.interpolatedVol(forwardPriceMarket, observedOptionPeriod(observationDay), Some(observationDay), Some(strike), isIndexVol = true, Some(averagePrice))
  }

  def fixing(env : InstrumentLevelEnvironment, observationDay : Day) = {
    env.quantity(FixingKey(this, observationDay))
  }

  def forwardPrice(env: InstrumentLevelEnvironment, observationDay: Day, ignoreShiftsIfPermitted: Boolean) = {
    env.quantity(ForwardPriceKey(forwardPriceMarket, observedPeriod(observationDay), ignoreShiftsIfPermitted))
  }

  def fixingPeriod(day:Day) : FixingPeriod

  def fixing(slice: MarketDataSlice, observationDay : Day) = {
    val key = PriceFixingsHistoryDataKey(forwardPriceMarket)
    slice.fixings(key, ObservationPoint(observationDay, observationTimeOfDay))
      .fixingFor(level, fixingPeriod(observationDay).storedPeriod)
      .toQuantity
  }

  def convert(value: Quantity, uom: UOM): Option[Quantity] = {
    forwardPriceMarket.convert(value, uom)
  }

  def possiblePricingRules = List(NoPricingRule)

  def indexes = Set(this)
  def businessCalendar = forwardPriceMarket.businessCalendar
}

object SingleIndex{
  lazy val commodityMarketToIndex : Map[CommodityMarket, SingleIndex] = {
    PublishedIndex.publishedIndexes.map{
      idx => idx.market -> idx
    }.toMap
  }
}

case class PublishedIndex(
  indexName: String,
  eaiQuoteID: Option[Int],
  market: CommodityMarket,
  indexLevel: Level = Level.Mid
)
  extends SimpleSingleIndex(indexName, market, level = indexLevel)
{
  def this(indexName: String, eaiQuoteID: Int, market: CommodityMarket) = this(indexName, Some(eaiQuoteID), market)

  def observedPeriod(day : Day) = market match {
    case f: FuturesMarket => f.frontPeriod(day)
    case f: ForwardMarket => f.underlying(day)
  }
  def fixingPeriod(day: Day) = DateRangeFixingPeriod(observedPeriod(day))

  def observedOptionPeriod(observationDay: Day) = market match {
    case fm: FuturesMarket => fm.frontOptionPeriod(observationDay)
    case fm: ForwardMarket => fm.underlyingOption(observationDay)
  }
}

object PublishedIndex{

  // Fur unit tests
  def apply(name : String, market : CommodityMarket) : PublishedIndex = PublishedIndex(name, None, market)

  /**
   * Published Indexes
   */
  val ROTTERDAM_BARGES = new PublishedIndex("3.5% Fuel FOB Rotterdam Barges", 5, Market.FUEL_FOB_ROTTERDAM_BARGES_3_5)
//  val LBMA_GOLD_AM = new PublishedIndex("Gold AM", 804, Market.LBMA_GOLD)
//  val LBMA_GOLD_AVG = new PublishedIndex("Gold Avg", 804, Market.LBMA_GOLD) // TODO [07 Jul 2010] This needs to be an average!
//  val LBMA_GOLD_PM = new PublishedIndex("Gold PM", 847, Market.LBMA_GOLD)
//  val SILVER_BULL = new PublishedIndex("Silver (Bull)", 805, Market.LBMA_SILVER) // TODO [07 Jul 2010] what's the difference between these 2?
//  val SILVER_LOW_4_LME = new PublishedIndex("Silver low 4 LME", 805, Market.LBMA_SILVER)
  val No_6_3PC_USGC_Waterborne = new PublishedIndex("No.6 3% USGC Waterborne", 11, Market.No_6_3PC_USGC_Waterborne)
  val UNL_87_USGC_PIPELINE = new PublishedIndex("Unl 87 USGC Pipeline", 34, Market.UNL_87_USGC_PIPELINE)
  val HSFO_180_CST_Singapore = new PublishedIndex("HSFO 180 CST Singapore", 8, Market.HSFO_180_CST_Singapore)
  val HSFO_380_CST_Singapore = new PublishedIndex("HSFO 380 CST Singapore", 134, Market.HSFO_380_CST_Singapore)
  val PREM_UNL_FOB_ROTTERDAM_BARGES = new PublishedIndex("Prem Unl FOB Rotterdam Barges", 17, Market.PREM_UNL_FOB_ROTTERDAM_BARGES)

  val FUEL_FOB_NWE_CARGOES_1 = new PublishedIndex("1% Fuel FOB NWE Cargoes", 3, Market.FUEL_FOB_NWE_CARGOES_1)
  val NAPHTHA_CIF_NWE_CARGOES = new PublishedIndex("Naphtha CIF NWE Cargoes", 37, Market.NAPHTHA_CIF_NWE_CARGOES)
  val GAS_OIL_0_5_SINGAPORE = new PublishedIndex("Gas Oil 0.5 Singapore", 52, Market.GAS_OIL_0_5_SINGAPORE)
  val MOGAS_95_UNL_10PPM_NWE_BARGES = new PublishedIndex("Mogas 95 Unl 10ppm NWE Barges (Argus)", 88, Market.MOGAS_95_UNL_10PPM_NWE_BARGES)
  val UNL_92_SINGAPORE_CARGOES = new PublishedIndex("Unl 92 Singapore Cargoes", 198, Market.UNL_92_SINGAPORE_CARGOES)
  val GAS_OIL_0_1_FOB_ROTTERDAM_BARGES = new PublishedIndex("Gas Oil 0.1% FOB Rotterdam Barges (Platts)", 1011, Market.GAS_OIL_0_1_FOB_ROTTERDAM_BARGES)
  val GAS_OIL_ULSD_USGC_PIPELINE = new PublishedIndex("Gas Oil ULSD USGC Pipeline (Platts)", 1039, Market.GAS_OIL_ULSD_USGC_PIPELINE)
  val GAS_OIL_0_1_CIF_NWE_CARGOES = new PublishedIndex("Gas Oil 0.1% CIF NWE Cargoes (Platts)", 1049, Market.GAS_OIL_0_1_CIF_NWE_CARGOES)
  val PREM_UNL_10PPM_FOB_MED_CARGOES = new PublishedIndex("Prem Unl 10ppm FOB Med Cargoes (Platts)", 1183, Market.PREM_UNL_10PPM_FOB_MED_CARGOES)
  val PREM_UNL_EURO_BOB_OXY_NWE_BARGES = new PublishedIndex("Prem Unl Euro-Bob Oxy NWE Barges (Argus)", 1312, Market.PREM_UNL_EURO_BOB_OXY_NWE_BARGES)
  val JET_CIF_NWE_CARGOES = new PublishedIndex("Jet CIF NWE Cargoes", 18, Market.JET_CIF_NWE_CARGOES)
  val GAS_OIL_ULSD_10PPM_CIF_NWE_CARGOES = new PublishedIndex("Gas Oil ULSD 10ppm CIF NWE Cargoes", 598, Market.GAS_OIL_ULSD_10PPM_CIF_NWE_CARGOES)
  val GAS_OIL_ULSD_10PPM_FOB_ROTTERDAM_BARGES = new PublishedIndex("Gas Oil ULSD 10ppm FOB Rotterdam Barges", 883, Market.GAS_OIL_ULSD_10PPM_FOB_ROTTERDAM_BARGES)

  // TODO [08 Apr 2011] -- WRONG! -- We have changed the market from PLATTS_BRENT to DATED_BRENT because the latter has the matching LIM Symbol
  val FORTIES_CRUDE_1ST_MONTH = new PublishedIndex("Forties Crude 1st month (Platts)", 332, Market.DATED_BRENT)

  val DATED_BRENT = new PublishedIndex("Dated Brent", 40, Market.DATED_BRENT)
  val URALS_CIF_MED = new PublishedIndex("Urals CIF Med Recombined (RCMB)", 159, Market.URALS_CIF_MED)

  val PLATTS_BRENT: Map[BrentMonth, PublishedIndex] = {
    (1 to 12).map {
      i => {
        val month = new BrentMonth(i)
        (month -> new PublishedIndex("Platts " + month, None, Market.PLATTS_BRENT_MONTH_MARKETS(month), indexLevel = Level.MidPoint)
    )
      }
    }
  }.toMap

  val PANAMAX_TC_AVG = PublishedIndex("Panamax T/C Avg", Some(511), Market.BALTIC_PANAMAX, Level.Val)
  val SUPRAMAX_TC_AVG = new PublishedIndex("Supramax T/C Avg", Some(1306), Market.BALTIC_SUPRAMAX, Level.Val)
  val CAPSIZE_TC_AVG = new PublishedIndex("Capsize T/C Avg", Some(512), Market.BALTIC_CAPESIZE, Level.Val)
  val C7_TC_AVG = new PublishedIndex("C7 T/C Avg", Some(524), Market.BALTIC_CAPESIZE_C7, Level.Val)

  val publishedIndexes:List[PublishedIndex] = List(
    ROTTERDAM_BARGES,
    No_6_3PC_USGC_Waterborne,
    UNL_87_USGC_PIPELINE,
    HSFO_180_CST_Singapore,
    PREM_UNL_FOB_ROTTERDAM_BARGES,
    FUEL_FOB_NWE_CARGOES_1,
    NAPHTHA_CIF_NWE_CARGOES,
    GAS_OIL_0_5_SINGAPORE,
    MOGAS_95_UNL_10PPM_NWE_BARGES,
    UNL_92_SINGAPORE_CARGOES,
    GAS_OIL_0_1_FOB_ROTTERDAM_BARGES,
    GAS_OIL_ULSD_USGC_PIPELINE,
    GAS_OIL_0_1_CIF_NWE_CARGOES,
    PREM_UNL_10PPM_FOB_MED_CARGOES,
    PREM_UNL_EURO_BOB_OXY_NWE_BARGES,
    DATED_BRENT,
    FORTIES_CRUDE_1ST_MONTH,
    URALS_CIF_MED,
    PANAMAX_TC_AVG,
    SUPRAMAX_TC_AVG,
    CAPSIZE_TC_AVG,
    C7_TC_AVG,
    JET_CIF_NWE_CARGOES,
    GAS_OIL_ULSD_10PPM_CIF_NWE_CARGOES,
    GAS_OIL_ULSD_10PPM_FOB_ROTTERDAM_BARGES,
    HSFO_380_CST_Singapore
  ) ::: PLATTS_BRENT.map(_._2).toList

  val eaiQuoteMap = publishedIndexes.toMapWithSomeKeys(_.eaiQuoteID)
  val marketToPublishedIndexMap = publishedIndexes.toMapWithKeys(_.market)
}

case class FuturesFrontPeriodIndex(
  val market : FuturesMarket,
  rollBeforeDays : Int = 0,
  promptness : Int = 1
 ) extends SimpleSingleIndex(
  "%s %s %s price" % (market.name, FuturesFrontPeriodIndex.promptnessString(promptness), market.tenor.toString.toLowerCase),
  market,
  level = Level.Close
 ){

  def observedPeriod(observationDay : Day) : DateRange = {
    val frontMonth = market.frontPeriod(observationDay.addBusinessDays(market.businessCalendar, rollBeforeDays))
    val period = frontMonth match {
      case month:Month => month + (promptness - 1)
      case other => other //This happens in a test but may not be needed when run with real data
    }
    period
  }
  def fixingPeriod(observationDay: Day) = DateRangeFixingPeriod(observedPeriod(observationDay))

  def observedOptionPeriod(observationDay: Day) = market.frontOptionPeriod(
    observationDay.addBusinessDays(market.businessCalendar, rollBeforeDays)
  )

  def frontFuturesMonthToSwapMonth(frontFuturesMonth : Month) : Month = {
    market.lastTradingDay(frontFuturesMonth).containingMonth
  }

  override def reportDisplayName = "%s 1st %s" % (market.name, market.tenor.toString.toLowerCase)
}

object FuturesFrontPeriodIndex{
  val WTI10 = new FuturesFrontPeriodIndex(Market.NYMEX_WTI, 0, 1)
  val WTI20 = new FuturesFrontPeriodIndex(Market.NYMEX_WTI, 0, 2)
  val ICEWTI10 = new FuturesFrontPeriodIndex(Market.ICE_WTI, 0, 1)

  val BRT11 = new FuturesFrontPeriodIndex(Market.ICE_BRENT, 1, 1)
  val GO11  = new FuturesFrontPeriodIndex(Market.ICE_GAS_OIL, 1, 1)
  val HO10  = new FuturesFrontPeriodIndex(Market.NYMEX_HEATING, 0, 1)
  val RBOB10  = new FuturesFrontPeriodIndex(Market.NYMEX_GASOLINE, 0, 1)
  val COMEX_HG_COPPER_1ST_MONTH  = new FuturesFrontPeriodIndex(Market.COMEX_HIGH_GRADE_COPPER, 0, 1)
  val COMEX_HG_COPPER_2ND_MONTH  = new FuturesFrontPeriodIndex(Market.COMEX_HIGH_GRADE_COPPER, 0, 2)
  val NYMGO11 = new FuturesFrontPeriodIndex(Market.ICE_GAS_OIL, 1, 1) {
    override def lotSize = Some(1000.0)
  }

  val PLDUB10 = new FuturesFrontPeriodIndex(Market.PLATTS_DUBAI, 1, 0)

  val PLATTS_BRENT_1ST_MONTH = new FuturesFrontPeriodIndex(Market.PLATTS_BRENT, 0, 0)
  val PLATTS_BRENT_2ND_MONTH = new FuturesFrontPeriodIndex(Market.PLATTS_BRENT, 1, 0)
  val PLATTS_BRENT_3RD_MONTH = new FuturesFrontPeriodIndex(Market.PLATTS_BRENT, 2, 0)

  /*
    The ICE Brent one-minute marker. A weighted average of all trades over a specified minute
    during the day. We have no Lim Symbol for this that I can find, but only american options
    (at least in book 173), so a lack of fixings shouldn't be an issue. The forward curve should
    be the same as that for the standard brent futures index.
   */
  val BRT1M11 = new FuturesFrontPeriodIndex(Market.ICE_BRENT, 1, 1)

  val eaiQuoteMap: Map[Int, FuturesFrontPeriodIndex] = Map(
  // these first two are a bit strange, they are the ids for the futures market but some swaps are booked against them.
  // the swaps act like they are booked against the futures front period for these markets
    1 -> BRT11,
    2 -> WTI10,

    7 -> WTI10,
    15 -> HO10,
    28 -> BRT11,
    317 -> PLATTS_BRENT_1ST_MONTH,
    1091 -> ICEWTI10,
    1215 -> PLATTS_BRENT_2ND_MONTH,
    1336 -> PLATTS_BRENT_3RD_MONTH,
    29 -> HO10,
    58 -> GO11,
    933 -> RBOB10,
    1281 -> BRT1M11,
    1431 -> NYMGO11,
    6 -> PLDUB10
    )
  lazy val knownFrontFuturesIndices: List[FuturesFrontPeriodIndex] = List(
    WTI10,
    WTI20,
    ICEWTI10,
    BRT11,
    GO11,
    HO10,
    RBOB10,
    NYMGO11,
    BRT1M11,
    COMEX_HG_COPPER_1ST_MONTH,
    COMEX_HG_COPPER_2ND_MONTH
    )
  lazy val unknownFrontFuturesIndices: List[FuturesFrontPeriodIndex] = {
    val knownMarkets = knownFrontFuturesIndices.map(_.market)
    Market.futuresMarkets.filterNot(knownMarkets.contains).map(m=>FuturesFrontPeriodIndex(m))
  }

  lazy val futuresMarketToIndexMap = Map(
    Market.ICE_BRENT -> BRT11,
    Market.ICE_GAS_OIL -> GO11, // TODO [29 Jun 2010] should this be NYMGO11
    Market.NYMEX_GASOLINE -> RBOB10,
    Market.NYMEX_HEATING -> HO10,
    Market.NYMEX_WTI -> WTI10,
    Market.ICE_WTI -> ICEWTI10
  )

  def promptnessString(promptness: Int): String = {
    if(promptness <= 1)
      "front"
    else if (promptness == 2)
      "second"
    else
      throw new Exception("Unrecognised promptness: " + promptness)
  }
}


abstract class MultiIndex(override val name: CaseInsensitive) extends Index(name) {
  /**
   * To avoid blank rows cash needs to be associated with a risk market and period. 
   * Doesn't really amtter which we use - just making sure the choice is consistent
   */
  lazy val arbitraryIndexToAssignCashTo = indexes.toList.sortWith(_.toString < _.toString).head

  def averagePrice(env : Environment, averagingPeriod: DateRange, rule: SwapPricingRule, priceUOM: UOM): Quantity

  def markets = indexes.flatMap(_.markets).toList

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

  val lmeIndices = Market.marketsForExchange(FuturesExchangeFactory.LME).map(LmeCashSettlementIndex)

  val indicesToImportFixingsForFromEAI : List[SimpleSingleIndex] =
    FuturesFrontPeriodIndex.knownFrontFuturesIndices :::
    FuturesFrontPeriodIndex.unknownFrontFuturesIndices :::
    PublishedIndex.publishedIndexes

  val namedIndexes : List[Index] =
    FuturesFrontPeriodIndex.knownFrontFuturesIndices :::
    FuturesFrontPeriodIndex.unknownFrontFuturesIndices :::
    PublishedIndex.publishedIndexes :::
    FuturesSpreadIndex.spreadIndexes :::
    FormulaIndexList.formulaIndexes :::
    BrentCFDSpreadIndex.named.values.toList :::
    lmeIndices

  def fromNameOption(name : String) = namedIndexes.find(_.name == name)

  def fromName(name : String) = namedIndexes.find(_.name == name) match {
    case Some(index) => index
    case None => throw new UnknownIndexException("No index with name " + name + " in " + namedIndexes)
  }
  def singleIndexFromName(name: String) = fromName(name) match {
    case si: SingleIndex => si
    case other => throw new Exception(other + " is not of type TrinityIndex")
  }

  lazy val eaiQuoteMap : Map[Int, Index]=
    FuturesFrontPeriodIndex.eaiQuoteMap ++
    PublishedIndex.eaiQuoteMap ++
    FormulaIndexList.eaiQuoteMap

  def indexFromEAIQuoteID(id: Int): Index = eaiQuoteMap.get(id) match {
    case Some(i:FormulaIndex) => i.verify
    case Some(i) => i
    case None => {
      throw new UnknownIndexException(id + " is not a known Index eaiQuoteID", Some(id))
    }
  }

  def indexOptionFromEAIQuoteID(id: Int) = eaiQuoteMap.get(id)

  def unapply(eaiQuoteID: Int) = indexOptionFromEAIQuoteID(eaiQuoteID)

  def singleIndexFromEAIQuoteID(id: Int): SingleIndex = eaiQuoteMap.get(id) match {
    case Some(i:SingleIndex) => i
    case Some(i) => throw new UnknownIndexException(id + " is not a SingleIndex: " + i, Some(id))
    case None => throw new UnknownIndexException(id + " is not a known Index eaiQuoteID", Some(id))
  }
}


case class LmeCashSettlementIndex(futuresMarket : FuturesMarket) extends SimpleSingleIndex("LME " + futuresMarket.commodity + " cash", futuresMarket, level = Level.Ask){
  def observedOptionPeriod(observationDay: Day) = throw new Exception("Options not supported for LME indices")

  def observedPeriod(day : Day) = {
    assert(isObservationDay(day), day + " is not an observation day for " + this)
    day.addBusinessDays(businessCalendar, 2)
  }

  def fixingPeriod(day: Day) = new FixingPeriod(){
    def storedPeriod = StoredFixingPeriod.tenor(Tenor.cash)

    def period(day: Day) = observedPeriod(day)
  }

  override def observationTimeOfDay = ObservationTimeOfDay.Official
}

case class LmeThreeMonthBuyerIndex(futuresMarket : FuturesMarket) extends SimpleSingleIndex("LME " + futuresMarket.commodity + " 3m Buyer", futuresMarket, level = Level.Bid){
  def observedOptionPeriod(observationDay: Day) = throw new Exception("Options not supported for LME indices")

  def observedPeriod(day : Day) = {
    assert(isObservationDay(day), day + " is not an observation day for " + this)
    throw new Exception("Implement me")
  }

  def fixingPeriod(day: Day) = new FixingPeriod(){
    def storedPeriod = StoredFixingPeriod.tenor(Tenor.ThreeMonths)

    def period(day: Day) = observedPeriod(day)
  }

  override def observationTimeOfDay = ObservationTimeOfDay.Official
}
