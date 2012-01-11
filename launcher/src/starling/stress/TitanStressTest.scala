package starling.stress

import starling.startserver.Server
import starling.trade.facility.TradeFacility
import starling.auth.Client
import starling.fc2.api.FC2Facility
import starling.reports.facility.ReportFacility
import starling.props.Props
import starling.api.utils.PropertyValue
import starling.bouncyrmi.{BouncyRMI, BouncyRMIClient}
import starling.http.GUICode
import collection.mutable.ListBuffer
import starling.gui.api._
import starling.pivot._
import starling.utils.{Stopwatch, ThreadUtils}
import java.util.Random
import collection.immutable.TreeSet
import starling.tradestore.TradePredicate
import starling.manager.{TimeTree, Profiler}
import starling.daterange.{Timestamp, Day}
import starling.launcher.DevLauncher

/**
 * Does various tests but the main idea for most of them is this: 200 clients make a server request at a random time within a 10 second window.
 * The time it takes for all clients to finish is recorded as well as the longest time for a single client to get it's result back.
 */
object TitanStressTest {
  def initialSetupAndReturnProps = {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    val props0 = DevLauncher.propsWithUnusedPort()

    val source = "stress"
    val propsMap = props0.starlingProps +
      ("ImportMarketDataAutomatically" -> PropertyValue(source, "false")) +
      ("RabbitEnabled" -> PropertyValue(source, "false")) +
      ("TitanRabbitBrokerHost" -> PropertyValue(source, "")) +
      ("ImportsBookClosesFromEAI" -> PropertyValue(source, "false")) +
      ("QlikViewEnabled" -> PropertyValue(source, "false"))

    val props = new Props(propsMap, props0.trafiguraProps)

    System.setProperty(BouncyRMI.CodeVersionKey, GUICode.allMD5s)
    System.setProperty("appname", props.ServerName())

    props
  }

  def main(args:Array[String]) {
    val props = initialSetupAndReturnProps
    val run = Server.run(props)

    val day = Day(2012, 1, 6)
    val stopwatch = Stopwatch()

    // *** Market data reading

    val (tradeFacility1, fc2Facility1, reportFacility1) = createTradeFC2AndReportFacility(props)
    val pricingGroup = PricingGroup.Metals
    val latestMarketDataVersion = fc2Facility1.init().pricingGroupLatestMarketDataVersions(pricingGroup)
    val marketDataSelection = MarketDataSelection(Some(pricingGroup), None)
    val marketDataIdentifier = MarketDataIdentifier(marketDataSelection, latestMarketDataVersion)
    val marketDataPageIdentifier = StandardMarketDataPageIdentifier(marketDataIdentifier)

    val twoHundredTradeFC2sAndReportFacilities = (0 until 200).map(_ => createTradeFC2AndReportFacility(props))
    val random = new Random(12345L)

    val pricePivotFieldsState01 = new PivotFieldsState(
      filters = List((Field("Observation Day"), SomeSelection(Set(day)))),
      rowFields = List(Field("Market"), Field("Observation Time"), Field("Period")),
      columns = ColumnTrees.dataField(Field("Price")))
    val pricePivotFieldParams01 = PivotFieldParams(true, Some(pricePivotFieldsState01))
    val priceMarketDataLabel = MarketDataTypeLabel("Price")

    val numOfReads = 3
    stopwatch.reset()
    readMarketDataOnce(fc2Facility1, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01)
    val firstClientFirstReadOfPriceMarketData = stopwatch.asStringAndReset
    readMarketDataOnce(fc2Facility1, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01)
    val firstClientSecondReadOfPriceMarketData = stopwatch.asStringAndReset
    readMarketDataXTimes(numOfReads, fc2Facility1, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01).thread.join()
    val firstClientReadingPriceMarketDataXTimes = stopwatch.asStringAndReset

    val fc2Facility2 = createTradeFC2AndReportFacility(props)._2
    readMarketDataOnce(fc2Facility2, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01)
    val secondClientFirstReadOfPriceMarketData = stopwatch.asStringAndReset
    readMarketDataOnce(fc2Facility2, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01)
    val secondClientSecondReadOfPriceMarketData = stopwatch.asStringAndReset
    readMarketDataXTimes(numOfReads, fc2Facility2, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01).thread.join()
    val secondClientReadingPriceMarketDataXTimes = stopwatch.asStringAndReset

    val t1 = readMarketDataXTimes(numOfReads, fc2Facility1, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01)
    val t2 = readMarketDataXTimes(numOfReads, fc2Facility2, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01)
    t1.thread.join()
    t2.thread.join()
    val firstAndSecondClientReadingPriceMarketDataXTimes = stopwatch.asStringAndReset

    val tenFC2s = (0 until 10).map(_ => createTradeFC2AndReportFacility(props)._2)
    stopwatch.reset()
    val tenFC2sResults = tenFC2s.map(fc2 => readMarketDataXTimes(numOfReads, fc2, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01))
    tenFC2sResults.foreach(_.thread.join())
    val tenClientsReadingPriceMarketDataXTimes = stopwatch.asStringAndReset
    val maximumReadTimeForTenClients = Stopwatch.milliToHumanString(tenFC2sResults.map(_.maxMillis()).max)

    val twoHundredFC2s = twoHundredTradeFC2sAndReportFacilities.map(_._2)
    stopwatch.reset()
    val twoHundredFC2Results = twoHundredFC2s.map(fc2 => readMarketDataXTimes(numOfReads, fc2, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01))
    twoHundredFC2Results.foreach(_.thread.join())
    val twoHundredClientsReadingPriceMarketDataXTimes = stopwatch.asStringAndReset
    val maximumReadTimeForTwoHundredClients = Stopwatch.milliToHumanString(twoHundredFC2Results.map(_.maxMillis()).max)

    stopwatch.reset()
    val twoHundredReadingAfterDelayResults = twoHundredFC2s.map(fc2 => readMarketDataAfterDelay(fc2, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01, random.nextInt(10000).toLong))
    twoHundredReadingAfterDelayResults.foreach(_.thread.join())
    val twoHundredClientsReadingAfterDelay = stopwatch.asStringAndReset
    val maximumReadTimeForTwoHundredClientsWithDelay = Stopwatch.milliToHumanString(twoHundredReadingAfterDelayResults.map(_.maxMillis()).max)


    val fixingsPivotFieldsState = new PivotFieldsState(
      filters = List((Field("Observation Day"), SomeSelection(Set(day))), (Field("Market"), SomeSelection(Set("Copper")))),
      rowFields = List(Field("Exchange"), Field("Level"), Field("Period")),
      columns = ColumnTrees.dataField(Field("Price")))
    val fixingsPivotFieldParams = PivotFieldParams(true, Some(fixingsPivotFieldsState))
    val fixingsMarketDataLabel = MarketDataTypeLabel("PriceFixingsHistory")

    stopwatch.reset()
    val twoHundredClientsReadingFixingsForThe1stTimeAfterDelayResults = twoHundredFC2s.map(fc2 => readMarketDataAfterDelay(fc2, marketDataPageIdentifier, fixingsMarketDataLabel, fixingsPivotFieldParams, random.nextInt(10000).toLong))
    twoHundredClientsReadingFixingsForThe1stTimeAfterDelayResults.foreach(_.thread.join())
    val twoHundredClientsReadingFixingsForThe1stTimeAfterDelay = stopwatch.asStringAndReset
    val maxFixingsTimeWithDelay = Stopwatch.milliToHumanString(twoHundredClientsReadingFixingsForThe1stTimeAfterDelayResults.map(_.maxMillis()).max)


    val countryBenchmarkPivotFieldsState = new PivotFieldsState(
      filters = List((Field("Observation Day"), SomeSelection(Set("n/a"))), (Field("Observation Time"), SomeSelection(Set("Real Time")))),
      rowFields = List(Field("Commodity"), Field("Country"), Field("Grade"), Field("Effective Month")),
      columns = ColumnTrees.dataField(Field("Benchmark Price")))
    val benchmarkPivotFieldParams = PivotFieldParams(true, Some(countryBenchmarkPivotFieldsState))
    val benchmarkMarketDataLabel = MarketDataTypeLabel("CountryBenchmark")

    stopwatch.reset()
    val countryBenchmark1stTime1stClientResult = readMarketDataXTimes(1, fc2Facility1, marketDataPageIdentifier, benchmarkMarketDataLabel, benchmarkPivotFieldParams)
    val countryBenchmark1stTime2ndClientResult = readMarketDataXTimes(1, fc2Facility2, marketDataPageIdentifier, benchmarkMarketDataLabel, benchmarkPivotFieldParams)
    countryBenchmark1stTime1stClientResult.thread.join()
    countryBenchmark1stTime2ndClientResult.thread.join()
    val twoClientsReadingCountryBenchmarksFor1stTime = stopwatch.asStringAndReset
    val twoClientsReadingCountryBenchmarksFor1stTime1stClientTime = Stopwatch.milliToHumanString(countryBenchmark1stTime1stClientResult.maxMillis())
    val twoClientsReadingCountryBenchmarksFor1stTime2ndClientTime = Stopwatch.milliToHumanString(countryBenchmark1stTime2ndClientResult.maxMillis())

    stopwatch.reset()
    val twoHundredClientsReadingBenchmarksAfterDelayResults = twoHundredFC2s.map(fc2 => readMarketDataAfterDelay(fc2, marketDataPageIdentifier, benchmarkMarketDataLabel, benchmarkPivotFieldParams, random.nextInt(10000).toLong))
    twoHundredClientsReadingBenchmarksAfterDelayResults.foreach(_.thread.join())
    val twoHundredClientsReadingBenchmarksAfterDelay = stopwatch.asStringAndReset
    val maxBenchmarkReadTimeWithDelay = Stopwatch.milliToHumanString(twoHundredClientsReadingBenchmarksAfterDelayResults.map(_.maxMillis()).max)


    val pricePivotFieldsState02 = new PivotFieldsState(
      filters = List((Field("Observation Day"), SomeSelection(Set(day)))),
      rowFields = List(Field("Observation Time"), Field("Market"), Field("Period")),
      columns = ColumnTrees.dataField(Field("Price")))
    val pricePivotFieldParams02 = PivotFieldParams(true, Some(pricePivotFieldsState02))

    val pricePivotFieldsState03 = new PivotFieldsState(
      filters = List((Field("Observation Day"), SomeSelection(Set(day)))),
      rowFields = List(Field("Observation Time"), Field("Period"), Field("Market")),
      columns = ColumnTrees.dataField(Field("Price")))
    val pricePivotFieldParams03 = PivotFieldParams(true, Some(pricePivotFieldsState03))

    val pricePivotFieldsState04 = new PivotFieldsState(
      filters = List((Field("Observation Day"), SomeSelection(Set(day)))),
      rowFields = List(Field("Period"), Field("Observation Time"), Field("Market")),
      columns = ColumnTrees.dataField(Field("Price")))
    val pricePivotFieldParams04 = PivotFieldParams(true, Some(pricePivotFieldsState04))

    val pricePivotFieldsState05 = new PivotFieldsState(
      filters = List((Field("Observation Day"), SomeSelection(Set(day)))),
      rowFields = List(Field("Period"), Field("Market"), Field("Observation Time")),
      columns = ColumnTrees.dataField(Field("Price")))
    val pricePivotFieldParams05 = PivotFieldParams(true, Some(pricePivotFieldsState05))
    val priceFieldParams = List(
      pricePivotFieldParams01,
      pricePivotFieldParams02,
      pricePivotFieldParams03,
      pricePivotFieldParams04,
      pricePivotFieldParams05
    )

    stopwatch.reset()
    val twoHundredClientsReadingPricesWithDifferentFieldStatesWithDelayResults = twoHundredFC2s.map(fc2 => {
      readMarketDataAfterDelay(fc2, marketDataPageIdentifier, priceMarketDataLabel, priceFieldParams(random.nextInt(5)), random.nextInt(10000).toLong)
    })
    twoHundredClientsReadingPricesWithDifferentFieldStatesWithDelayResults.foreach(_.thread.join())
    val twoHundredClientsReadingPricesWithDifferentFieldStatesWithDelay = stopwatch.asStringAndReset
    val maxPriceReadWithDelayDifferentFieldStates = Stopwatch.milliToHumanString(twoHundredClientsReadingPricesWithDifferentFieldStatesWithDelayResults.map(_.maxMillis()).max)


    val marketDataLabels = List(
      (priceMarketDataLabel, pricePivotFieldParams01),
      (benchmarkMarketDataLabel, benchmarkPivotFieldParams),
      (fixingsMarketDataLabel, fixingsPivotFieldParams))
    stopwatch.reset()
    val twoHundredClientsReadingDifferentTypesOfMarketDataWithDelayResults = twoHundredFC2s.map(fc2 => {
      val (marketDataLabel, params) = marketDataLabels(random.nextInt(3))
      readMarketDataAfterDelay(fc2, marketDataPageIdentifier, marketDataLabel, params, random.nextInt(10000).toLong)
    })
    twoHundredClientsReadingDifferentTypesOfMarketDataWithDelayResults.foreach(_.thread.join())
    val twoHundredClientsReadingDifferentTypesOfMarketDataWithDelay = stopwatch.asStringAndReset
    val maxReadTypesWithDelay = Stopwatch.milliToHumanString(twoHundredClientsReadingDifferentTypesOfMarketDataWithDelayResults.map(_.maxMillis()).max)


    // *** Single trade valuations

    val mods = TreeSet[EnvironmentModifierLabel](EnvironmentModifierLabel.zeroInterestRates)
    val observationDay = day.endOfDay
    val curveIdentifierLabel = CurveIdentifierLabel(
      marketDataIdentifier,
      EnvironmentRuleLabel.MostRecentCloses,
      observationDay,
      observationDay,
      observationDay.day.nextWeekday.endOfDay,
      mods
    )
    val tradeIDPivotFieldState = new PivotFieldsState(rowFields = List(Field("Trade ID")))
    val tradeIDParams = PivotFieldParams(true, Some(tradeIDPivotFieldState))
    val desk = Desk.Titan
    val deskCloses = tradeFacility1.init().deskCloses(desk)(TradeTimestamp.magicLatestTimestampDay)
    val tradeTimestamp = deskCloses.sortWith(_.timestamp > _.timestamp).head
    val allTradeSelection = TradeSelectionWithTimestamp(Some((desk, tradeTimestamp)), TradePredicate.Null, None)
    val tradeIDsPivotData = tradeFacility1.tradePivot(allTradeSelection, day.startOfFinancialYear, tradeIDParams)
    val tradeIDs = tradeIDsPivotData.pivotTable.rowAxis.map(_.axisValue.value.value).filter(_.isInstanceOf[TradeIDLabel]).map(_.asInstanceOf[TradeIDLabel]).filter(id => {
      id.id.startsWith("A") || id.id.startsWith("Q")
    })
    val numTradeIDs = tradeIDs.size
    val tradeID = tradeIDs(0)
    val tradePredicate = TradePredicate(List(), List(List((Field("Trade ID"), SomeSelection(Set(tradeID))))))
    val tradeSelectionSingleTrade = TradeSelectionWithTimestamp(Some((desk, tradeTimestamp)), tradePredicate, None)
    val timestamp = tradeSelectionSingleTrade.deskAndTimestamp.get._2.timestamp
    val twoHundredReportFacilities = twoHundredTradeFC2sAndReportFacilities.map(_._3)

    stopwatch.reset()
    runTradeValuationOnce(reportFacility1, tradeID, curveIdentifierLabel, timestamp)
    val singleTradeValuation1stTime = stopwatch.asStringAndReset

    runTradeValuationOnce(reportFacility1, tradeID, curveIdentifierLabel, timestamp)
    val singleTradeValuation2ndTime = stopwatch.asStringAndReset

    stopwatch.reset()
    val twoHundredClientsValuingSameTradeIDResults = twoHundredReportFacilities.map(reportFacility => runTradeValuationAfterDelay(reportFacility, tradeID, curveIdentifierLabel, timestamp, random.nextInt(10000).toLong))
    twoHundredClientsValuingSameTradeIDResults.map(_.thread.join())
    val twoHundredClientsValuingSameTradeID = stopwatch.asStringAndReset
    val maxReadValuingSameTradeID = Stopwatch.milliToHumanString(twoHundredClientsValuingSameTradeIDResults.map(_.maxMillis()).max)

    val tradeID2 = tradeIDs(1)
    stopwatch.reset()
    runTradeValuationOnce(reportFacility1, tradeID2, curveIdentifierLabel, timestamp)
    val singleDifferentTradeValuation = stopwatch.asStringAndReset

    val twoHundredClientsValuingRandomTradesResults = twoHundredReportFacilities.map(reportFacility => {
      runTradeValuationAfterDelay(reportFacility, tradeIDs(random.nextInt(numTradeIDs)), curveIdentifierLabel, timestamp, random.nextInt(10000).toLong)
    })
    twoHundredClientsValuingRandomTradesResults.map(_.thread.join())
    val twoHundredClientsValuingRandomTrades = stopwatch.asStringAndReset
    val maxReadForRandomTrades = maxSingleReadText(twoHundredClientsValuingRandomTradesResults)

    // *** Full reports

    val reportPivotFieldState1 = new PivotFieldsState(
      rowFields = List(Field("Risk Market"), Field("Risk Period")),
      columns = ColumnTrees.dataField(Field("Position"))
    )
    val reportPivotFieldParams1 = PivotFieldParams(true, Some(reportPivotFieldState1))
    val prl = reportFacility1.reportOptionsAvailable.options.filter(_.slidable)
    val reportOptions1 = ReportOptions(prl,None,None)
    val reportParameters1 = ReportParameters(allTradeSelection, curveIdentifierLabel, reportOptions1, day.startOfFinancialYear, None, true)

    stopwatch.reset()
    runReportOnce(reportFacility1, reportParameters1, reportPivotFieldParams1)
    val firstClientFirstReport = stopwatch.asStringAndReset

    runReportOnce(reportFacility1, reportParameters1, reportPivotFieldParams1)
    val secondClientFirstReport = stopwatch.asStringAndReset

    stopwatch.reset()
    val twoHundredReportsResultsWithDelay = twoHundredReportFacilities.map(reportFacility => {
      runReportAfterDelay(reportFacility, reportParameters1, reportPivotFieldParams1, random.nextInt(10000).toLong)})
    twoHundredReportsResultsWithDelay.map(_.thread.join())
    val twoHundredClientsRunningSameReportWithDelay = stopwatch.asStringAndReset
    val maxRunReportTimeWithDelay = maxSingleReadText(twoHundredReportsResultsWithDelay)

    val reportPivotFieldState2 = new PivotFieldsState(
      rowFields = List(Field("Risk Period"), Field("Risk Market")),
      columns = ColumnTrees.dataField(Field("Position"))
    )
    val reportPivotFieldParams2 = PivotFieldParams(true, Some(reportPivotFieldState2))

    stopwatch.reset()
    runReportOnce(reportFacility1, reportParameters1, reportPivotFieldParams2)
    val firstClientSameReportDiffPivotFieldState = stopwatch.asStringAndReset

    val twoHundredClientsRunningSameReportAndLayoutAsAboveResults = twoHundredReportFacilities.map(reportFacility => {
      runReportAfterDelay(reportFacility, reportParameters1, reportPivotFieldParams2, random.nextInt(10000).toLong)})
    twoHundredClientsRunningSameReportAndLayoutAsAboveResults.map(_.thread.join())
    val twoHundredClientsRunningSameReportAndLayoutAsAbove = stopwatch.asStringAndReset
    val maxRun200ClientsSameReportAndLayoutAsAbove = maxSingleReadText(twoHundredClientsRunningSameReportAndLayoutAsAboveResults)

    val potentialRowFields = List(Field("Risk Market"), Field("Risk Period"), Field("Instrument"), Field("Trade ID"), Field("Risk Type"), Field("Risk Commodity"))
    val potentialMeasureFields = List(Field("Position"), Field("Market Price"), Field("Current Price"), Field("P&L"))
    def randomPivotFieldParams = {
      val numRowFields = random.nextInt(potentialRowFields.size)
      val rowFields = (0 until numRowFields).map(_ => potentialRowFields(random.nextInt(potentialRowFields.size))).distinct.toList
      val numMeasureFields = random.nextInt(potentialMeasureFields.size)
      val measureFields = (0 until numMeasureFields).map(_ => potentialMeasureFields(random.nextInt(potentialMeasureFields.size))).distinct.toList
      val columnTrees =  ColumnTrees(measureFields.map(mf => new ColumnTree(FieldOrColumnStructure(mf, true), ColumnTrees.Null)))
      val pfs = new PivotFieldsState(rowFields = rowFields, columns = columnTrees)
      PivotFieldParams(true, Some(pfs))
    }
    stopwatch.reset()
    val twoHundredClientsRunningSameReportWithDifferentLayoutsResults = twoHundredReportFacilities.map(reportFacility => {
      runReportAfterDelay(reportFacility, reportParameters1, randomPivotFieldParams, random.nextInt(10000).toLong)
    })
    twoHundredClientsRunningSameReportWithDifferentLayoutsResults.map(_.thread.join())
    val twoHundredClientsRunningSameReportWithDifferentLayouts = stopwatch.asStringAndReset
    val maxSingleReadFor200ClientsRunningTheSameReportWithDifferentLayouts = maxSingleReadText(twoHundredClientsRunningSameReportWithDifferentLayoutsResults)

    val twoHundredClientsRunningSameReportWithDifferentLayoutsWithMarketDataImportResults = twoHundredReportFacilities.map(reportFacility => {
      runReportAfterDelay(reportFacility, reportParameters1, randomPivotFieldParams, random.nextInt(10000).toLong)
    })
    fc2Facility1.importData(marketDataSelection, day)
    twoHundredClientsRunningSameReportWithDifferentLayoutsWithMarketDataImportResults.map(_.thread.join())
    val maxSingleReadFor200ClientsRunningTheSameReportWithDifferentLayoutsWithMarketDataImport = maxSingleReadText(twoHundredClientsRunningSameReportWithDifferentLayoutsWithMarketDataImportResults)

    // *** Tidy up and output

    println("Stoppings clients")
    clients.foreach(_.stop())
    println("Stopping server")
    run.stop()
    println("")
    ThreadUtils.printNonDaemonThreads
    println("")
    println("")
    println("")
    println("")

    val output = List(
      ("1st client first read of price market data:", firstClientFirstReadOfPriceMarketData),
      ("1st client second read of price market data:", firstClientSecondReadOfPriceMarketData),
      ("1st client reading price market data " + numOfReads + " times:", firstClientReadingPriceMarketDataXTimes),
      ("2nd client first read of price market data:", secondClientFirstReadOfPriceMarketData),
      ("2nd client second read of price market data:", secondClientSecondReadOfPriceMarketData),
      ("2nd client reading price market data " + numOfReads + " times:", secondClientReadingPriceMarketDataXTimes),
      ("1st & 2nd client reading price market data " + numOfReads + " times:", firstAndSecondClientReadingPriceMarketDataXTimes),
      ("10 clients reading price market data " + numOfReads + " times:", tenClientsReadingPriceMarketDataXTimes),
      ("10 clients reading price market data " + numOfReads + " times max single read:", maximumReadTimeForTenClients),
      ("200 clients reading price market data " + numOfReads + " times:", twoHundredClientsReadingPriceMarketDataXTimes),
      ("200 clients reading price market data " + numOfReads + " times max single read:", maximumReadTimeForTwoHundredClients),
      ("200 clients reading price market data with random delay up to 10 seconds:", twoHundredClientsReadingAfterDelay),
      ("200 clients reading price market data with random delay up to 10 seconds max single read:", maximumReadTimeForTwoHundredClientsWithDelay),
      ("2 clients reading benchmarks for the first time:", twoClientsReadingCountryBenchmarksFor1stTime),
      ("2 clients reading benchmarks for the first time - 1st client time:", twoClientsReadingCountryBenchmarksFor1stTime1stClientTime),
      ("2 clients reading benchmarks for the first time - 2nd client time:", twoClientsReadingCountryBenchmarksFor1stTime2ndClientTime),
      ("200 clients reading fixings for 1st time with random delay up to 10 seconds:", twoHundredClientsReadingFixingsForThe1stTimeAfterDelay),
      ("200 clients reading fixings for 1st time with random delay up to 10 seconds max single read:", maxFixingsTimeWithDelay),
      ("200 clients reading benchmarks with random delay up to 10 seconds:", twoHundredClientsReadingBenchmarksAfterDelay),
      ("200 clients reading benchmarks with random delay up to 10 seconds max single read:", maxBenchmarkReadTimeWithDelay),
      ("200 clients reading prices with random delay up to 10 seconds and different field states:", twoHundredClientsReadingPricesWithDifferentFieldStatesWithDelay),
      ("200 clients reading prices with random delay up to 10 seconds and different field states max single read:", maxPriceReadWithDelayDifferentFieldStates),
      ("200 clients reading different types of market data with random delay up to 10 seconds:", twoHundredClientsReadingDifferentTypesOfMarketDataWithDelay),
      ("200 clients reading different types of market data with random delay up to 10 seconds max single read:", maxReadTypesWithDelay),
      ("", ""),
      ("Single trade valuation 1st time:", singleTradeValuation1stTime),
      ("Single trade valuation 2nd time:", singleTradeValuation2ndTime),
      ("200 clients valuing same trade with random delay up to 10 seconds:", twoHundredClientsValuingSameTradeID),
      ("200 clients valuing same trade random delay up to 10 seconds max single read:", maxReadValuingSameTradeID),
      ("Single client valuing different trade:", singleDifferentTradeValuation),
      ("200 clients valuing random trades random delay up to 10 seconds:", twoHundredClientsValuingRandomTrades),
      ("200 clients valuing random trades random delay up to 10 seconds max single read:", maxReadForRandomTrades),
      ("", ""),
      ("1st client running report 1st time:", firstClientFirstReport),
      ("1st client running report 2nd time:", secondClientFirstReport),
      ("200 clients running the same report with random delay up to 10 seconds:", twoHundredClientsRunningSameReportWithDelay),
      ("200 clients running the same report with random delay up to 10 seconds max single read:", maxRunReportTimeWithDelay),
      ("1st client running the same report with different pivot field state:", firstClientSameReportDiffPivotFieldState),
      ("200 clients running the same report and layout as above with random delay up to 10 seconds:", twoHundredClientsRunningSameReportAndLayoutAsAbove),
      ("200 clients running the same report and layout as above with random delay up to 10 seconds max single read:", maxRun200ClientsSameReportAndLayoutAsAbove),
      ("200 clients running the same report with different layouts with random delay up to 10 seconds:", twoHundredClientsRunningSameReportWithDifferentLayouts),
      ("200 clients running the same report with different layouts with random delay up to 10 seconds max single read:", maxSingleReadFor200ClientsRunningTheSameReportWithDifferentLayouts),
      ("200 clients running the same report with different layouts and market data import with random delay up to 10 seconds max single read:", maxSingleReadFor200ClientsRunningTheSameReportWithDifferentLayoutsWithMarketDataImport),
      ("", "")
    )
    val maxLength = output.map(_._1.length()).max + 1
    val maxResultLength = output.map(_._2.length()).max
    val formattedOutput = output.map{case (description, result) => {(description.padTo(maxLength, " ").mkString, result)}}

    println("Results".padTo(maxLength + maxResultLength, "-").mkString)
    formattedOutput.foreach{case (description, result) => println(description + result)}
    println("-".padTo(maxLength + maxResultLength, "-").mkString)
  }

  def readMarketDataOnce(fc2Facility:FC2Facility, marketDataPageIdentifier:MarketDataPageIdentifier,
                         marketDataType:MarketDataTypeLabel, pivotFieldParameters:PivotFieldParams) = {
    Profiler.captureTime("Read market data once") {
      fc2Facility.readAllMarketData(marketDataPageIdentifier, Some(marketDataType), PivotEdits.Null, pivotFieldParameters)
    }
  }

  def readMarketDataXTimes(x:Int, fc2Facility:FC2Facility, marketDataPageIdentifier:MarketDataPageIdentifier,
                              marketDataType:MarketDataTypeLabel, pivotFieldParameters:PivotFieldParams) = {
    @volatile var maxReadTime = 0L

    val thread = new Thread {
      override def run() {
        val stopwatch = Stopwatch()
        val timings = new ListBuffer[Long]()
        for (c <- 0 until x) {
          stopwatch.reset()
          readMarketDataOnce(fc2Facility, marketDataPageIdentifier, marketDataType, pivotFieldParameters)
          timings += stopwatch.ms()
        }
        maxReadTime = timings.max
      }
    }
    thread.start()
    ReadResult(thread, () => maxReadTime, () => null)
  }

  def readMarketDataAfterDelay(fc2Facility:FC2Facility, marketDataPageIdentifier:MarketDataPageIdentifier,
                                marketDataType:MarketDataTypeLabel, pivotFieldParameters:PivotFieldParams, delay:Long) = {
    @volatile var maxReadTime = 0L
    @volatile var timeTree:TimeTree = null

    val thread = new Thread {
      override def run() {
        Thread.sleep(delay)
        val stopwatch = Stopwatch()
        val (_, tTree) = readMarketDataOnce(fc2Facility, marketDataPageIdentifier, marketDataType, pivotFieldParameters)
        maxReadTime = stopwatch.ms()
        timeTree = tTree
      }
    }
    thread.start()
    ReadResult(thread, () => maxReadTime, () => timeTree)
  }

  def runReportAfterDelay(reportFacility:ReportFacility, reportParameters:ReportParameters,
                          pivotFieldParameters:PivotFieldParams, delay:Long) = {
    @volatile var maxReadTime = 0L
    @volatile var timeTree:TimeTree = null

    val thread = new Thread {
      override def run() {
        Thread.sleep(delay)
        val stopwatch = Stopwatch()
        val (_, tTree) = runReportOnce(reportFacility, reportParameters, pivotFieldParameters)
        maxReadTime = stopwatch.ms()
        timeTree = tTree
      }
    }
    thread.start()
    ReadResult(thread, () => maxReadTime, () => timeTree)
  }

  def runReportOnce(reportFacility:ReportFacility, reportParameters:ReportParameters, pivotFieldParameters:PivotFieldParams) = {
    Profiler.captureTime("run report once") {
      reportFacility.reportPivot(reportParameters, pivotFieldParameters)
    }
  }

  def runTradeValuationAfterDelay(reportFacility:ReportFacility, tradeID:TradeIDLabel, curveIdentifier:CurveIdentifierLabel,
                                  timestamp:Timestamp, delay:Long) = {
    @volatile var maxReadTime = 0L
    @volatile var timeTree:TimeTree = null

    val thread = new Thread {
      override def run() {
        Thread.sleep(delay)
        val stopwatch = Stopwatch()
        val (_, tTree) = runTradeValuationOnce(reportFacility, tradeID, curveIdentifier, timestamp)
        maxReadTime = stopwatch.ms()
        timeTree = tTree
      }
    }
    thread.start()
    ReadResult(thread, () => maxReadTime, () => timeTree)
  }

  def runTradeValuationOnce(reportFacility:ReportFacility, tradeID:TradeIDLabel, curveIdentifier:CurveIdentifierLabel, timestamp:Timestamp) = {
    Profiler.captureTime("run trade valuation once") {
      reportFacility.tradeValuation(tradeID, curveIdentifier, timestamp, ReportSpecificChoices())
    }
  }

  val clients = new ListBuffer[BouncyRMIClient]()

  private def createClient(props:Props) = {
    val client = new BouncyRMIClient("localhost", props.RmiPort(), Client.Null)
    client.startBlocking()
    clients += client
    client
  }

  private def createTradeFC2AndReportFacility(props:Props) = {
    val client = createClient(props)
    (client.proxy(classOf[TradeFacility]), client.proxy(classOf[FC2Facility]), client.proxy(classOf[ReportFacility]))
  }

  def maxSingleReadText(readResults:IndexedSeq[ReadResult]) = {
    Stopwatch.milliToHumanString(readResults.map(_.maxMillis()).max) +
      " - average: " + Stopwatch.milliToHumanString(readResults.map(_.maxMillis()).sum / readResults.size)
  }

  def maxSingleReadTextNoDesc(readResults:IndexedSeq[ReadResult]) = {
    readResults.map(_.maxMillis()).max + "\t" + (readResults.map(_.maxMillis()).sum / readResults.size)
  }
}

case class ReadResult(thread:Thread, maxMillis: ()=>Long, timeTree: ()=>TimeTree)