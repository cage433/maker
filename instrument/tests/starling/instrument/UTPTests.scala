package starling.instrument

import starling.utils.StarlingTest
import starling.quantity.UOM._
import starling.quantity.Quantity._
import org.testng.annotations.{DataProvider, Test}
import starling.utils.QuantityTestUtils._
import org.testng.Assert._
import starling.curves._
import interestrate.{DayCountActual365}
import starling.utils.Reflection
import starling.market.{InterestRateMarket, Index, Market}
import starling.market._
import starling.utils.CaseInsensitive._
import starling.richdb.RichInstrumentResultSetRow
import java.lang.reflect.{Method, InvocationHandler, Proxy}
import collection.immutable.TreeSet
import starling.daterange._
import starling.daterange.Day._
import starling.models.{European, Put, Call}
import cern.colt.matrix.impl.DenseDoubleMatrix2D
import starling.quantity.{UOM, Percentage, Quantity}
import starling.curves.ForwardPriceKey
import starling.calendar.BrentMonth

class UTPTests extends IndexTest {

  val zeroRates = Map(USD -> 0.05, GBP -> 0.02, EUR -> 0.08)
  val xRates = Map(GBP -> 0.7,EUR -> 0.9)
  lazy val leadMarket = Market.LME_LEAD
  val unlIndex = Index.PREM_UNL_EURO_BOB_OXY_NWE_BARGES
  lazy val brentIndex = Index.DATED_BRENT
  lazy val jan = new BrentMonth(1)
  lazy val cfdIndex = BrentCFDSpreadIndex.indexFor(jan)
  lazy val plattsJan = Index.publishedIndexFromName("Platts Brent (" + jan.monthName + ")")
  val env = Environment(
    new TestingAtomicEnvironment(){
      def marketDay = Day(2009, 2, 10).endOfDay

      def applyOrMatchError(key: AtomicDatumKey) = {
        key match {
          case DiscountRateKey(ccy, day, _) =>  math.exp(- zeroRates(ccy) * day.daysSinceInYears(marketDay.day))
          case ForwardPriceKey(Market.NYMEX_WTI, Month(2010, 10), _) => 100 (Market.NYMEX_WTI.priceUOM) //for CSO
          case ForwardPriceKey(Index.DATED_BRENT, _, _) => 98 (USD/BBL)
          case ForwardPriceKey(`plattsJan`, _, _) => 95 (USD/BBL)
          case ForwardPriceKey(market, day, _) => 96 (market.priceUOM)
          case FixingKey(`brentIndex`, _) => 97(USD/BBL)
          case FixingKey(index, _) => 96(index.priceUOM)
          case _ : BradyMetalVolAtomicDatumKey => Percentage(0.3)
          case _ : BradyFXVolSmileAtomicDatumKey => Map(0.5 -> Percentage(0.3))
          case _ : OilAtmVolAtomicDatumKey => Percentage(0.10)
          case _ : OilVolSkewAtomicDatumKey => Map(0.5 -> Percentage(0.1))
          case _ : EquityPriceKey => 100 (USD/SHARE)
          case USDFXRateKey(ccy) => xRates(ccy)(USD/ccy)
          case SpreadAtmStdDevAtomicDatumKey(market, period, ignoreShifts) => 10 (market.priceUOM)
          case SpreadSkewStdDevAtomicDatumKey(market, period) => {
            val matrix = new DenseDoubleMatrix2D(1, 2)
            matrix.set(0, 0, 0.0); matrix.set(0, 1, 5.0)
            matrix
          }
        }
      }
    }
    )

  @DataProvider(name = "tradeableProvider")
  def tradeableProvider : Array[Array[Tradeable]] = {
    Array[Tradeable](
      Future(leadMarket, Day(2009, 8, 3), 123(USD/MT), 550 (MT)),
      CommoditySwap(unlIndex, 123(USD/MT), 777(MT), Month(2010, 1), cleared = false),
      // multiple months
      CommoditySwap(unlIndex, 123(USD/MT), 777(MT), Quarter(2010, 1), cleared = false),
      // mid period
      CommoditySwap(unlIndex, 123(USD/MT), 777(MT), Quarter(2009, 1), cleared = false),

    // TODO CFDs don't work at the moment. we need to spend some time on them.
      CFD(cfdIndex, -(1(USD/BBL)), 777(BBL), Month(2009, 2)), // mid period
      CFD(cfdIndex, -(1(USD/BBL)), 777(BBL), Month(2008, 12)), // all before
      CFD(cfdIndex, -(1(USD/BBL)), 1000(BBL), Month(2009, 10)), // all after

      new FuturesOption(leadMarket, Day(2009, 8, 1), Day(2009, 8, 20), 90(USD/MT), 333(MT), Call, European),
      AsianOption(unlIndex, Month(2009, 2), 98(USD/MT), 222(MT), Put),
      AsianOption(unlIndex, Strip(Month(2009, 1), Month(2009, 10)), 98(USD/MT), 222(MT), Put),
      Future(leadMarket, Day(2009, 9, 9), 77(USD/MT), 111(MT)),
      FXForward(1.5(EUR/USD), 999(USD), Day(2009, 9, 8)),
      FXOption(1.3(EUR/USD), 999(USD), Day(2009, 9, 8), Day(2009, 10, 10), Put),
      new CalendarSpreadOption(Market.NYMEX_WTI, Spread(Month(2010, 11), Month(2010, 12)), Quantity(-1, USD/BBL), Quantity(10000, BBL), Call),
      new CalendarSpreadOption(Market.NYMEX_WTI, Spread(Month(2011, 1), Month(2011, 2)), Quantity(-1, USD/BBL), Quantity(10000, BBL), Call),
      new CalendarSpreadOption(Market.NYMEX_WTI, Spread(Month(2010, 10), Month(2010, 11)), Quantity(0.2, USD/BBL), Quantity(10000, BBL), Call),
      new FuturesOption(Market.LME_ALUMINIUM, Day(2009, 8, 1), Day(2009, 8, 20),
        Quantity(90, USD/MT), Quantity(100, MT), Call, European),
      new NetEquityPosition(RIC("Foo"), Quantity(100, UOM.SHARE)),

      // Future Spreads
      new FuturesCalendarSpread(Market.NYMEX_WTI, Month(2011, 1), Month(2011, 2), Quantity(55, USD/BBL), Quantity(56, USD/BBL), Quantity(1000, BBL)),
      new FuturesCalendarSpread(Market.NYMEX_WTI, Month(2011, 1), Month(2011, 2), Quantity(1, USD/BBL), Quantity(1000, BBL)),

      new FuturesCommoditySpread(FuturesSpreadMarket.RB_CRACKS, Month(2011, 1), Quantity(55, USD/BBL), Quantity(56, USD/BBL), Quantity(1000, BBL)),
      new FuturesCommoditySpread(FuturesSpreadMarket.RB_CRACKS, Month(2011, 1), Quantity(-1, USD/BBL), Quantity(1000, BBL)),

      // Swap Spreads
      new SwapCalendarSpread(brentIndex, Quantity(.9, USD/BBL), Quantity(1000, BBL), SpreadPeriod(DateRange(1 Jan 2011, 13 Jan 2011), Month(2011, 3)), cleared = true),

      RefinedAssignment(leadMarket, Day(2010, 1, 1), Quantity(100, MT)),
      RefinedFixationsForSplit(List(RefinedFixation(leadMarket, Day(2010, 1, 1), "Y", Quantity(100, MT)))),

      CashInstrument(CashInstrumentType.Ordinary, Quantity(100, USD), Day(2011, 1, 1)),

      // Commodity Spread Options
      CommoditySpreadOption(FuturesSpreadMarket.ICE_WTI_BRENT, Month(2011, 1), Quantity(-1, USD/BBL), Quantity(1000, BBL), Call)

    ).map(Array[Tradeable](_))
  }

  @DataProvider(name = "instrumentProvider")
  def instrumentProvider:Array[Array[UTP]] = {
    tradeableProvider.flatMap(_(0).asUtpPortfolio(Day(2009, 1, 1)).portfolio.keySet).map(Array(_))
  }

  @Test(dataProvider = "tradeableProvider")
  def testMtmOfInstrumentMatchesThatOfUTP(tradeable : Tradeable){
    if (tradeable.isInstanceOf[Instrument]) { //FXForward doesn't have a mtm method
      val mtm = tradeable.asInstanceOf[Instrument].mtm(env, USD)
      val utp = tradeable.asUtpPortfolio(Day(2009, 1, 1))
      val utpMtm = utp.mtm(env, USD)
      assertQtyEquals(mtm, utpMtm, 1e-6)
      assertTrue(math.abs(mtm.value) > 1.0, "sample mtm is too small " + mtm.value)
    }
  }

  @Test
  def testWeHaveCheckedAllInstruments{
    var tradeableTypes = TradeableType.types.filterNot(_ == DeletedInstrument).filterNot(_ == ErrorInstrument)
    var typesSeen = Set[TradeableType[_]]()
    tradeableProvider.foreach{
      inst =>
        val tradeableType = inst(0).asInstanceOf[Tradeable].tradeableType
        assertTrue(tradeableTypes.contains(tradeableType))
        typesSeen += tradeableType
    }
    val missingTypes = tradeableTypes.toList.filterNot(typesSeen.contains)
    assertTrue(
      missingTypes.isEmpty,
      "The tests for UTP portfolios are not complete, missing " + missingTypes
    )
  }

  private def resultSetRowFromMap(details:Map[String,Any]) = {
    val normalisedDetails = normaliseDetails(details)
    Proxy.newProxyInstance(
      classOf[UTPTests].getClassLoader,
      Array(classOf[RichInstrumentResultSetRow]),
      new InvocationHandler() {
        def invoke(proxy: Object, method: Method, args: Array[Object]):Object = {
          try {
            val key = if(args != null) {
              normailiseKey("" + args(0))
            } else {
              normailiseKey("no key")
            }
            if (method.getName == "getDateRange$default$2") {
              None
            } else if (method.getName == "getOptionType") {
              normalisedDetails(normailiseKey("exercisetype")).asInstanceOf[Object]
            } else if (method.getName == "getPeriod") {
              normalisedDetails(normailiseKey("period")).asInstanceOf[Object]
            } else if (method.getName == "getStrikes") {
              normalisedDetails(normailiseKey("initialprice")).asInstanceOf[Object]
            } else if (method.getName == "isNull") {
              (!normalisedDetails.contains(key) || normalisedDetails(key) == null).asInstanceOf[Object]
            } else {
              normalisedDetails(key).asInstanceOf[Object]
            }
          }
          catch {
            case e => {
              throw new Exception("Problem with proxy for " + method, e)
            }
          }
        }
      }
    ).asInstanceOf[RichInstrumentResultSetRow]
  }

  private def normaliseDetails(details:Map[String,Any]) = {
    details.map { kv => normailiseKey(kv._1) -> kv._2}
  }

  private def normailiseKey(name:String) = name.toLowerCase.replaceAll(" ", "")

  @Test(dataProvider = "tradeableProvider")
  def testWeCanReadAndWriteAllTradeables(tradeable : Tradeable) {
    val details = tradeable.tradeableDetails.map { kv => kv._1.toLowerCase.replaceAll(" ", "") -> kv._2}
    val fakeRow = resultSetRowFromMap(details)
    val created = tradeable.tradeableType.createTradeable(fakeRow)

    assertEquals(created, tradeable)
  }

  @Test(dataProvider = "tradeableProvider")
  def testUTPDetailsFieldsAreInInstrumentTypeKeysList(tradeable : Tradeable) {
    val utps = tradeable.asUtpPortfolio(Day(2009, 1, 1)).instruments
    for (utp <- utps) {
      val detailsKeys = normaliseDetails(utp.details).keySet
      val undeclaredFields = detailsKeys.toList.filterNot(InstrumentType.lowercaseNoSpaceFields contains _)
      assertTrue(undeclaredFields.isEmpty, "There are undeclared fields in " + utp.instrumentType + " " + undeclaredFields + " -- " +  InstrumentType.lowercaseNoSpaceFields)
    }
  }

  @Test(dataProvider = "tradeableProvider")
  def testTradeableDetailsFieldsAreInTradeableTypeKeysList(tradeable : Tradeable) {
    val detailsKeys = normaliseDetails(tradeable.tradeableDetails).keySet
    val undeclaredFields = detailsKeys.toList.filterNot(TradeableType.lowercaseNoSpaceFields contains _)
    assertTrue(undeclaredFields.isEmpty, "There are undeclared fields in " + tradeable + " " + undeclaredFields)
  }

  @Test
  def testThereAreNoSurplusKeysInInstrumentTypeFieldsList() {
    val instruments = tradeableProvider.flatMap(tradeables=>tradeables).flatMap{tradeable=>tradeable.asUtpPortfolio(Day(2009, 1, 1)).instruments}
    val allFields = (TreeSet[String]() ++ instruments.flatMap(_.details.keySet.map(_.replaceAll(" ", "").toLowerCase))) + "error"
    val actual = TreeSet[String]() ++ InstrumentType.fields.map(_.replaceAll(" ", "").toLowerCase)
    assertEquals(actual, allFields)
  }

  @Test
  def testThereAreNoSurplusKeysInTradeableTypeFieldsList() {
    val tradeables = tradeableProvider.flatMap(tradeables=>tradeables)
    val allFields = (TreeSet[String]() ++ tradeables.flatMap(_.tradeableDetails.keySet.map(_.replaceAll(" ", "").toLowerCase))) + "error"
    val actual = TreeSet[String]() ++ TradeableType.fields.map(_.replaceAll(" ", "").toLowerCase)
    assertEquals(actual, allFields)
  }

  @Test(dataProvider = "instrumentProvider")
  def testMtmOfForwardStateIsTheSameForZeroVols(utp : UTP) {
    // Refined Assignments currently can't perform forward state correctly. This is
    // due to our not currently having access to their fixings.
    utp match {
      case _ : RefinedAssignment => 
      case _ => {
        val zeroVolsEnv = env.zeroVols.undiscounted
        val forwardDayAndTime = DayAndTime(env.marketDay.day + 500, TimeOfDay.EndOfDay)
        val mtm = utp.mtm(zeroVolsEnv, USD)
        assertTrue(mtm.value > 0.1, utp + ": mtm must be positive to test the utp correctly " + mtm)
        val forwardEnv = zeroVolsEnv.forwardState(forwardDayAndTime)
        val forwardUtp = utp.forwardState(zeroVolsEnv, forwardDayAndTime)
        val forwardMtm = forwardUtp.mtm(forwardEnv, USD)
        assertQtyClose(forwardMtm, mtm, message = "forwardmtm " + forwardMtm + " != mtm " + mtm)
      }
    }
  }

  @Test(dataProvider = "instrumentProvider")
  def testInstrumentLevelPriceKeysCorrespondToExpectedAtomicOnes(utp : UTP){
    val (atomicPriceKeys, _) = UTP.priceAndVolKeys(utp, env.marketDay, showEquivalentFutures = true, tenor = Day)
    val (instLevelPriceKeys, _) = UTP.priceAndVolKeys(utp, env.marketDay, showEquivalentFutures = false, tenor = Day)
    def getAtomicPriceKeys(diff : EnvironmentDifferentiable with PriceKey) : Set[EnvironmentDifferentiable with PriceKey] = {
      val record = KeyRecordingCurveObjectEnvironment(new NullAtomicEnvironment(env.marketDay))
      val value = diff.quantityValue(Environment(record))
      record.keys.map(_.clearProperties).flatMap(EnvironmentDifferentiable.toEnvironmentDifferentiable).filter(_.isInstanceOf[EnvironmentDifferentiable with PriceKey]).asInstanceOf[Set[EnvironmentDifferentiable with PriceKey]]
    }

    val atomicPriceKeys2 = instLevelPriceKeys.flatMap(getAtomicPriceKeys)
    assertEquals(atomicPriceKeys2, atomicPriceKeys)
  }

}
