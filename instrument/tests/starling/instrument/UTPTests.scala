package starling.instrument

import physical.{UnallocatedSalesQuota, PhysicalMetalAssignment}
import starling.quantity.UOM._
import starling.quantity.Quantity._
import org.testng.annotations.{DataProvider, Test}
import starling.quantity.utils.QuantityTestUtils._
import org.testng.Assert._
import starling.curves._
import interestrate.{DayCountActual365}
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
import starling.instrument.utils.StarlingXStream
import starling.utils.{StarlingTest, Reflection}
import starling.utils.sql.PersistAsBlob

class UTPTests extends IndexTest {

  val zeroRates = Map(USD -> 0.05, GBP -> 0.02, EUR -> 0.08)
  val xRates = Map(GBP -> 0.7,EUR -> 0.9)
  lazy val wtiMarket = Market.NYMEX_WTI
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
          case DiscountRateKey(ccy, day, _) =>  new Quantity(math.exp(- zeroRates(ccy) * day.daysSinceInYears(marketDay.day)))
          case ForwardPriceKey(Market.NYMEX_WTI, Month(2010, 10), _) => 100 (Market.NYMEX_WTI.priceUOM) //for CSO
          case MarketFixingKey(Market.NYMEX_WTI, _, _) => 50 (Market.NYMEX_WTI.priceUOM) //for CSO
          case ForwardPriceKey(Index.DATED_BRENT, _, _) => 98 (USD/BBL)
          case ForwardPriceKey(`plattsJan`, _, _) => 95 (USD/BBL)
          case ForwardPriceKey(market, day, _) => 96 (market.priceUOM)
          case IndexFixingKey(`brentIndex`, _) => 97(USD/BBL)
          case IndexFixingKey(index, _) => 96(index.priceUOM)
          case _ : OilAtmVolAtomicDatumKey => Percentage(0.10)
          case _ : OilVolSkewAtomicDatumKey => Map(0.1 -> Percentage(0.05))
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
      Future(wtiMarket, Month(2009, 8), 123(USD/BBL), 550 (BBL)),
      CommoditySwap(unlIndex, 123(USD/MT), 777(MT), Month(2010, 1), cleared = false),
      // multiple months
      CommoditySwap(unlIndex, 123(USD/MT), 777(MT), Quarter(2010, 1), cleared = false),
      // mid period
      CommoditySwap(unlIndex, 123(USD/MT), 777(MT), Quarter(2009, 1), cleared = false),

      new FuturesOption(wtiMarket, Day(2009, 8, 1), Month(2009, 9), 90(USD/BBL), 333(BBL), Call, European),
      AsianOption(unlIndex, Month(2009, 2), 98(USD/MT), 222(MT), Put),
      AsianOption(unlIndex, Strip(Month(2009, 1), Month(2009, 10)), 98(USD/MT), 222(MT), Put),
      Future(wtiMarket, Month(2009, 9), 77(USD/BBL), 111(BBL)),
      FXForward(1.5(EUR/USD), 999(USD), Day(2009, 9, 8)),
      new CalendarSpreadOption(wtiMarket, Spread(Month(2010, 11), Month(2010, 12)), Quantity(-1, USD/BBL), Quantity(10000, BBL), Call),
      new CalendarSpreadOption(wtiMarket, Spread(Month(2011, 1), Month(2011, 2)), Quantity(-1, USD/BBL), Quantity(10000, BBL), Call),
      new CalendarSpreadOption(wtiMarket, Spread(Month(2010, 10), Month(2010, 11)), Quantity(0.2, USD/BBL), Quantity(10000, BBL), Call),
      new FuturesOption(wtiMarket, Day(2009, 8, 1), Month(2009, 9), Quantity(90, USD/BBL), Quantity(100, BBL), Call, European),

      // Future Spreads
      new FuturesCalendarSpread(wtiMarket, Month(2011, 1), Month(2011, 2), Quantity(55, USD/BBL), Quantity(56, USD/BBL), Quantity(1000, BBL)),
      new FuturesCalendarSpread(wtiMarket, Month(2011, 1), Month(2011, 2), Quantity(1, USD/BBL), Quantity(1000, BBL)),

      // TAS
      new TAS(wtiMarket, Month(2011, 1), Day(2008, 12, 1), Quantity(1000, BBL)),


      new FuturesCommoditySpread(FuturesSpreadMarket.RB_CRACKS, Month(2011, 1), Quantity(55, USD/BBL), Quantity(56, USD/BBL), Quantity(1000, BBL)),
      new FuturesCommoditySpread(FuturesSpreadMarket.RB_CRACKS, Month(2011, 1), Quantity(-1, USD/BBL), Quantity(1000, BBL)),

      // Swap Spreads
      new SwapCalendarSpread(brentIndex, Quantity(.9, USD/BBL), Quantity(1000, BBL), SpreadPeriod(DateRange(1 Jan 2011, 13 Jan 2011), Month(2011, 3)), cleared = true),

      RefinedAssignment(leadMarket, Day(2010, 1, 1), Quantity(100, MT)),
      RefinedFixationsForSplit(List(RefinedFixation(leadMarket, Day(2010, 1, 1), "Y", Quantity(100, MT)))),

      CashInstrument(CashInstrumentType.Ordinary, Quantity(100, USD), Day(2011, 1, 1)),

      // Commodity Spread Options
      CommoditySpreadOption(FuturesSpreadMarket.ICE_WTI_BRENT, Month(2011, 1), Quantity(-1, USD/BBL), Quantity(1000, BBL), Call),

      PhysicalMetalAssignment.sample,
      UnallocatedSalesQuota.sample

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
            } else if (method.getName == "getObject") {
              val entry = normalisedDetails(key).asInstanceOf[PersistAsBlob]
              val text = StarlingXStream.write(entry.obj)
              val read = StarlingXStream.read(text).asInstanceOf[Object]
              read
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
    val details = tradeable.persistedTradeableDetails.map { kv => kv._1.toLowerCase.replaceAll(" ", "") -> kv._2}
    val fakeRow = resultSetRowFromMap(details)
    val created = tradeable.tradeableType.createTradeable(fakeRow)

    assertEquals(created, tradeable)
  }

  @Test(dataProvider = "tradeableProvider")
  def testUTPDetailsFieldsAreInInstrumentTypeKeysList(tradeable : Tradeable) {
    val utps = tradeable.asUtpPortfolio(Day(2009, 1, 1)).instruments
    for (utp <- utps) {
      val detailsKeys = normaliseDetails(utp.detailsForUTPNOTUSED).keySet
      val undeclaredFields = detailsKeys.toList.filterNot(InstrumentType.lowercaseNoSpaceFields contains _)
      assertTrue(undeclaredFields.isEmpty, "There are undeclared fields in " + utp.instrumentType + " " + undeclaredFields + " -- " +  InstrumentType.lowercaseNoSpaceFields)
    }
  }

  @Test(dataProvider = "tradeableProvider")
  def testTradeableDetailsFieldsAreInTradeableTypeKeysList(tradeable : Tradeable) {
    val detailsKeys = normaliseDetails(tradeable.shownTradeableDetails).keySet
    val undeclaredFields = detailsKeys.toList.filterNot(TradeableType.lowercaseNoSpaceFields contains _)
    assertTrue(undeclaredFields.isEmpty, "There are undeclared fields in " + tradeable + " " + undeclaredFields)
  }

  @Test
  def testThereAreNoSurplusKeysInInstrumentTypeFieldsList() {
    val instruments : Seq[UTP] = tradeableProvider.flatMap(tradeables=>tradeables).flatMap{tradeable=>tradeable.asUtpPortfolio(Day(2009, 1, 1)).instruments}
    val allFields = (TreeSet[String]() ++ instruments.flatMap(_.fields.map(_.replaceAll(" ", "").toLowerCase))) + "error"
    val actual = TreeSet[String]() ++ InstrumentType.fields.map(_.replaceAll(" ", "").toLowerCase)
    if (actual != allFields){
      println("All - actual")
      (allFields -- actual).foreach(println)
      println("actual - all")
      (actual -- allFields).foreach(println)
    }
    assertEquals(actual, allFields, " missing/extra fields " + (actual -- allFields) + " " + (allFields -- actual))
  }

  @Test
  def testThereAreNoSurplusKeysInTradeableTypeFieldsList() {
    val tradeables = tradeableProvider.flatMap(tradeables=>tradeables)
    val allFields = (TreeSet[String]() ++ tradeables.flatMap(_.shownTradeableDetails.keySet.map(_.replaceAll(" ", "").toLowerCase))) + "error"
    val actual = TreeSet[String]() ++ TradeableType.fields.map(_.replaceAll(" ", "").toLowerCase)
    if (actual != allFields){
      println("All - actual")
      (allFields -- actual).foreach(println)
      println("actual - all")
      (actual -- allFields).foreach(println)
    }
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
        assertTrue(mtm.abs.value > 0.1, utp + ": mtm must be non-zero to test the utp correctly " + mtm)
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

  @Test(dataProvider = "tradeableProvider")
  def testExplanationValuation(tr : Tradeable){
    try {
      assertQtyEquals(
        tr.explanation(env),
        tr.asUtpPortfolio(Day(2009, 1, 1)).mtm(env, tr.valuationCCY),
        1e-7
      )
    } catch {
      case _ : UnsupportedOperationException => 
      case e => throw e
    }
  }
}
