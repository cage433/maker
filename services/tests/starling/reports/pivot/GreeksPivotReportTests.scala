package starling.reports.pivot

import greeks.{GreekValues, GreeksPivotReport}
import starling.utils.StarlingTest
import starling.curves.Environment
import starling.curves.NullAtomicEnvironment
import org.testng.annotations._
import starling.market.Market
import starling.quantity.Quantity
import starling.quantity.Quantity._
import starling.models.Call
import org.testng.Assert._
import starling.instrument.FuturesCalendarSpread
import starling.instrument.UTP
import starling.market.Index
import starling.instrument.Future
import starling.models.European
import starling.instrument.FuturesOption
import starling.instrument.CompositeInstrument
import starling.quantity.UOM
import starling.quantity.UOM._
import starling.utils.QuantityTestUtils._
import starling.curves.USDFXRateKey
import starling.curves.ForwardPriceKey
import starling.market.SingleIndex
import starling.curves.PriceDifferentiable
import starling.curves.ForwardCurveKey
import starling.curves.USDFXRateCurveKey
import starling.maths.RandomVariables
import starling.maths.RandomThing
import starling.market.PublishedIndex
import starling.instrument.SingleAsianOption
import starling.instrument.SingleCalendarSpreadOption
import starling.instrument.SinglePeriodSwap
import starling.utils.CollectionUtils
import starling.reports.pivot.PivotReport._
import starling.reports.pivot.greeks.GreeksPivotReport._
import starling.concurrent.MP
import starling.daterange.{Week, Day, Month, DateRange}
import starling.daterange.Day._
import starling.market.BrentCFDSpreadIndex
import starling.calendar.BrentMonth
import starling.utils.SummingMap
import starling.daterange.Period
import starling.daterange.TenorType
import starling.gui.api.{ReportSpecificChoices, UTPIdentifier}
import starling.curves.UnitTestingAtomicEnvironment
import starling.curves.OilAtmVolCurveKey
import starling.curves.OilAtmVolAtomicDatumKey
import starling.quantity.Percentage
import starling.curves.DiscountRateKey
import starling.curves.OilVolSkewAtomicDatumKey
import starling.market.rules.CommonPricingRule

class GreeksPivotReportTests extends StarlingTest {
  val marketDay = Day(2010, 1, 1)
  val env = Environment(new NullAtomicEnvironment(marketDay.endOfDay))

  private def allSameClass(klass : Class[_])(seq : Seq[AnyRef]) = {
    seq.forall{
      elt =>
        if (elt.getClass == klass)
          true
        else {
          false
        }
    }
  }

  private def isExpectedSize(size : Int)(seq : Seq[_]) = {
    if (seq.size == size)
      true
    else {
      false
    }
  }

  lazy val mkt =  Market.NYMEX_WTI
  lazy val List(month1, month2) = List( Month(2010, 7), Month(2010, 8))
  lazy val atmPrice = env.forwardPrice(mkt, Month(2010, 11))
  lazy val idx = Index.WTI10
  lazy val asian = SingleAsianOption(
    idx, Month(2010, 10), atmPrice, Quantity(100, idx.uom), Call)
  lazy val cso = SingleCalendarSpreadOption(mkt, Day(2010, 6, 6), month1, month2, Quantity(0, mkt.priceUOM), Quantity(10, mkt.uom), Call)
  lazy val futuresOption = FuturesOption(
    mkt, Day(2010, 9, 10), Month(2010, 10), atmPrice,
    Quantity(19, mkt.uom), Call, European
  )
  lazy val swap = SinglePeriodSwap(idx, Quantity(0, idx.priceUOM), Quantity(17, idx.uom), Month(2010, 10), cleared = false)
  lazy val futuresSpread = FuturesCalendarSpread(
    mkt, Month(2010, 10), Month(2010, 11), 
    Quantity(0, mkt.priceUOM), Quantity(30, mkt.priceUOM), Quantity(19, mkt.uom)
  )

  lazy val cfd = SinglePeriodSwap(
    BrentCFDSpreadIndex.indexFor(BrentMonth(4)),
    0.54 (USD/BBL),
    1030000 (BBL),
    DateRange(24 Feb 2011, 2 Mar 2011),
    true, CommonPricingRule
  )

  lazy val swapWithFunnyPeriod = swap.copy(period = DateRange(10 Oct 2010, 20 Oct 2010), cleared = true)

  @DataProvider(name = "utpProvider")
  def utpProvider = {
    Array[Array[UTP]](
      Array(asian),
      Array(futuresOption),
      Array(futuresSpread),
      Array(swap),
      Array(cso),
      Array(cfd),
      Array(swapWithFunnyPeriod)
    )
  }

  private def isPositionReportKey(key : AnyRef) = {
    key match {
      case _ : ForwardPriceKey => true
      case _ : USDFXRateKey => true
      case _ : ForwardCurveKey => true
      case _ : USDFXRateCurveKey => true
      case _ => false
    }
  }

  val tenors = List("D", "W", "M")

  @Test(dataProvider = "utpProvider")
  def testDeltas(utp : UTP){
    val utpID = UTPIdentifier(0) 
    val utps = Map(utpID -> utp)
    for (collapseOptions <- List(true);
         showEqFutures <- List(true, false);
         futuresAsSpreads <- List(false, true);
         tenor <- tenors)
    {
      val pivotReport = new GreeksPivotReport(env, utps)
      val rows = pivotReport.rows(utpID, utp)
      val combinedRows = pivotReport.combine(
        rows,
        ReportSpecificChoices(
          collapseOptions_str -> collapseOptions,
          showEqFutures_str -> showEqFutures,
          futuresAsSpreads_str -> futuresAsSpreads,
          tenor_str -> tenor.toString,
          positionOnly_str -> true
        ))
      val sumPosition = combinedRows.map{row => row.position.quantityValue.get * row.scale}.sum
      val diffs = combinedRows.map(_.diff.get).toSet.toList
      val sumPosition2 = diffs.map(utp.position(env, _)).sum
      assertQtyClose(sumPosition, sumPosition, 5e-3)

    }
  }


  @Test(dataProvider = "utpProvider")
  def testScaling(utp : UTP){
    val utpID = UTPIdentifier(0) 
    val utps = Map(utpID -> utp)
    for (collapseOptions <- List(true, false);
         showEqFutures <- List(true, false);
         tenor <- tenors)
    {
      val pivotReport = new GreeksPivotReport(env, utps)
      val rows = pivotReport.rows(utpID, utp)
      val settings = ReportSpecificChoices(
          collapseOptions_str -> collapseOptions, 
          showEqFutures_str -> showEqFutures,
          tenor_str -> tenor.toString,
          positionOnly_str -> true
        )
      var combinedRows = pivotReport.combine(rows, settings)
      val scale = 9.5;
      var scaledCombinedRows = pivotReport.combine(rows.map(_ * scale), settings)
      val sumDelta1 = combinedRows.map(_.scaledPosition.quantityValue.get).sum * scale
      val sumDelta2 = scaledCombinedRows.map(_.scaledPosition.quantityValue.get).sum
      assertQtyEquals(sumDelta1, sumDelta2, 1e-6)
    }
  }

  @Test
  def testFuturesToSpreads{
    val strategyIds = Map[String, String]() :: List("a", "b", "c").map{name => Map("strategy ID" -> name)}
    val months = Month(2010, 1) upto Month(2011, 6)
    val markets = List(Market.NYMEX_WTI, Market.NYMEX_BRENT, Market.NYMEX_GASOLINE)
    var seed = 12345
    val u = RandomVariables.standardUniform(9999)
    val randomMarkets = new RandomThing(markets, 34824)
    val randomMonths = new RandomThing(months, 10477)

    val futures = (for (i <- 0 to 200) yield {
      val mkt = randomMarkets.next
      Future(mkt, randomMonths.next, Quantity(0, mkt.priceUOM), Quantity((u.nextDouble - 0.5) * 100, mkt.uom))
    }).toList

    val csos = (for (i <- 0 to 200) yield {
      val mkt = randomMarkets.next
      val mth = randomMonths.next
      val exerciseDay = mth.firstDay - 10
      SingleCalendarSpreadOption(mkt, exerciseDay, mth, mth + 1, Quantity(0, mkt.priceUOM), Quantity((u.nextDouble - 0.5) * 100, mkt.uom), Call)
    }).toList
    val randomStrategies = new RandomThing(strategyIds, 43434)
    val utps : Map[UTPIdentifier, UTP] = (futures ::: csos).zipWithIndex.map{
      case (utp, id) => (new UTPIdentifier(id, randomStrategies.next) -> utp)
    }.toMap
      
    val pivotReport = new GreeksPivotReport(env, utps)
    val rows : List[GreekValues] = utps.flatMap{
      case(utpID, utp) => pivotReport.rows(utpID, utp)
    }.toList.map(_ * 10.0)

    val portfolio = CompositeInstrument(utps.valuesIterator.toList)
    for (futuresAsSpreads <- List(true, false)){
      val settings = ReportSpecificChoices(futuresAsSpreads_str -> futuresAsSpreads, positionOnly_str -> true)
      val combinedRows = pivotReport.combine(rows, settings)
      val combinedUTPs = combinedRows.map{row => (row.utp * row.scale)}
      val combinedPortfolio = CompositeInstrument(combinedUTPs)
      val originalPortfolioFromRows = CompositeInstrument(rows.map{row => row.utp * row.scale})
      for (
        key <- CollectionUtils.filterOnType[PriceDifferentiable](portfolio.atomicMarketDataKeys(env.marketDay))
      ){
        val delta = portfolio.firstOrderDerivative(env, key, UOM.USD)
        val combinedDelta = combinedPortfolio.firstOrderDerivative(env, key, UOM.USD)
        val originalDelta = originalPortfolioFromRows.firstOrderDerivative(env, key, UOM.USD)
        assertQtyEquals(originalDelta, combinedDelta, 1e-6)
      }
    }
  }


  @Test
  def testFuturesToSwaps{
    val strategyIds = Map[String, String]() :: List("a", "b", "c").map{name => Map("strategy ID" -> name)}
    val months = Month(2010, 1) upto Month(2011, 6)
    val markets = List(Market.NYMEX_WTI, Market.NYMEX_BRENT, Market.NYMEX_GASOLINE, Market.LME_ZINC)
    var seed = 12345
    val u = RandomVariables.standardUniform(9999)
    val randomMarkets = new RandomThing(markets, 34824)
    val randomMonths = new RandomThing(months, 10477)

    val futures = (for (i <- 0 to 200) yield {
      val mkt = randomMarkets.next
      val period = if (mkt.tenor == Month) randomMonths.next else randomMonths.next.firstDay + 15
      Future(mkt, period, Quantity(0, mkt.priceUOM), Quantity((u.nextDouble - 0.5) * 100, mkt.uom))
    }).toList

    val swaps = markets.map{
      mkt => 
        val index : SingleIndex = Index.futuresMarketToIndexMap.get(mkt) match {
          case Some(idx) => idx
          case _ => PublishedIndex(mkt.name, mkt.eaiQuoteID, mkt.lotSize, mkt.uom, mkt.currency, mkt.businessCalendar, mkt.commodity)
        }
        SinglePeriodSwap(index, Quantity(0, mkt.priceUOM), Quantity((u.nextDouble - 0.5) * 100, mkt.uom), randomMonths.next, cleared = false)
    }
          
    val randomStrategies = new RandomThing(strategyIds, 43434)
    val utps : Map[UTPIdentifier, UTP] = (futures ::: swaps).zipWithIndex.map{
      case (utp, id) => (new UTPIdentifier(id, randomStrategies.next) -> utp)
    }.toMap
      
    val pivotReport = new GreeksPivotReport(env, utps)
    val rows : List[GreekValues] = utps.flatMap{
      case(utpID, utp) => pivotReport.rows(utpID, utp)
    }.toList.map(_ * 10.0)

    val portfolio = CompositeInstrument(utps.valuesIterator.toList)
    for (futuresAsSwaps <- List(true)){
      val settings = ReportSpecificChoices(futuresAsSwaps_str -> futuresAsSwaps, positionOnly_str -> true)
      val combinedRows = pivotReport.combine(rows, settings)
      if (futuresAsSwaps)
        assert(combinedRows.size < rows.size, "Combining should reduce rows")
      val combinedUTPs = combinedRows.map{row => (row.utp * row.scale)}
      val combinedPortfolio = CompositeInstrument(combinedUTPs)
      val originalPortfolioFromRows = CompositeInstrument(rows.map{row => row.utp * row.scale})
      for (
        key <- CollectionUtils.filterOnType[PriceDifferentiable](portfolio.atomicMarketDataKeys(env.marketDay))
      ){
        val delta = portfolio.firstOrderDerivative(env, key, UOM.USD)
        val combinedDelta = combinedPortfolio.firstOrderDerivative(env, key, UOM.USD)
        val originalDelta = originalPortfolioFromRows.firstOrderDerivative(env, key, UOM.USD)
        assertQtyEquals(originalDelta, combinedDelta, 1e-6)
      }
    }
  }

  @Test
  def testFuturesVolume{
    val fut = Future(Market.NYMEX_WTI, Month(2011, 12), Quantity(0, USD/BBL), Quantity(1, BBL))
    val utpID = UTPIdentifier(0) 
    val utps = Map(utpID -> fut)
    val pivotReport = new GreeksPivotReport(env, utps)
    val rows : List[GreekValues] = utps.flatMap{
      case(utpID, utp) => pivotReport.rows(utpID, utp)
    }.toList.map(_ * 10.0)
    val combinedRows = pivotReport.combine(rows, ReportSpecificChoices())
    assertQtyEquals(Quantity(10, BBL), combinedRows(0).position.quantityValue.get * combinedRows(0).scale, 1e-6)
  }

  @Test
  def testCFDThatCrossesMonths{
    val utpID = UTPIdentifier(0) 
    val utps = Map(utpID -> cfd)
    val pivotReport = new GreeksPivotReport(env.undiscounted, utps)

    val rows = pivotReport.rows(utpID, cfd)
    def netPositions(tenor : TenorType) = {
      val combinedRows = pivotReport.combine(rows, new ReportSpecificChoices(Map(tenor_str -> tenor)))
      var positions = SummingMap[(String, Period)]();
      combinedRows.foreach{ row =>
        positions += (row.marketName, row.period.get) -> row.scaledPosition.quantityValue.get
      }
      positions
    }
    val datedBrent = Index.DATED_BRENT.name
    val platts_april = Index.publishedIndexFromName("Platts Brent (April)").name

    val w9_2011 = Week(2011, 9)
    val w8_2011 = Week(2011, 8)
    var positions = netPositions(Week)
    assertQtyEquals(positions((datedBrent, w8_2011)), 412000.0 (BBL), 1e-6)
    assertQtyEquals(positions((datedBrent, w9_2011)), 618000.0 (BBL), 1e-6)
    assertQtyEquals(positions((platts_april, w8_2011)), -412000.0 (BBL), 1e-6)
    assertQtyEquals(positions((platts_april, w9_2011)), -618000.0 (BBL), 1e-6)

    val feb = Month(2011, 2)
    val mar = Month(2011, 3)
    positions =  netPositions(Month)
    assertQtyEquals(positions((datedBrent, feb)), 618000.0 (BBL), 1e-6)
    assertQtyEquals(positions((datedBrent, mar)), 412000.0 (BBL), 1e-6)
    assertQtyEquals(positions((platts_april, feb)), -618000.0 (BBL), 1e-6)
    assertQtyEquals(positions((platts_april, mar)), -412000.0 (BBL), 1e-6)

    positions = netPositions(Day)
    for (day <- cfd.period.days;
         if day.isWeekday){
      assertQtyEquals(positions((platts_april, day)), -206000.0 (BBL), 1e-6)
      assertQtyEquals(positions((datedBrent, day)), 206000.0 (BBL), 1e-6)
    }
  }

  @Test
  def testSwapWithFunnyPeriod {
    val utpID = UTPIdentifier(0) 
    val utps = Map(utpID -> swapWithFunnyPeriod)
    val pivotReport = new GreeksPivotReport(env, utps)

    val rows = pivotReport.rows(utpID, swapWithFunnyPeriod)

    for (tenor <- List(Month, Day, Week)){
      val combinedRows = pivotReport.combine(rows, new ReportSpecificChoices(Map(tenor_str -> tenor)))
      val position = combinedRows.map(_.scaledPosition.quantityValue.get).sum
      assertQtyEquals(position, swapWithFunnyPeriod.volume, 1e-6)
    }
  }

  @Test
  def testNetAsianVegaWithAllSettings {

    val idx = Index.WTI10
    val fwdPrice = 100(idx.priceUOM)
    val asian = SingleAsianOption(
      idx, Month(2010, 10), fwdPrice, 100(idx.uom), Call)
    val marketDay = (1 Jan 2010).endOfDay
    val env = Environment(UnitTestingAtomicEnvironment(
      marketDay,
      {
        case _ : ForwardPriceKey => fwdPrice
        case _ : OilAtmVolAtomicDatumKey => Percentage(0.5)
        case _ : DiscountRateKey => new Quantity(1.0)
        case skewKey : OilVolSkewAtomicDatumKey => skewKey.nullValue
      }
    ))

    val utpID = UTPIdentifier(0) 
    val utps = Map(utpID -> asian)
    val pivotReport = new GreeksPivotReport(env, utps)
    val rows = pivotReport.rows(utpID, asian)

    for ( showEqFutures <- List(true, false);
          atmVega <- List(true, false))
    {
      val combinedRows = pivotReport.combine(rows, new ReportSpecificChoices(Map(
        atmVega_str -> atmVega, showEqFutures_str -> showEqFutures
      )))
      val netVega =  combinedRows.map(_.vega.quantityValue.getOrElse(Quantity.NULL)).sum
      assertQtyEquals(netVega, 0.343(USD), 1e-3)
    }

  }
}

