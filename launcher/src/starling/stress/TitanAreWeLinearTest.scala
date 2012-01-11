package starling.stress

import TitanStressTest._
import starling.startserver.Server
import collection.mutable.ListBuffer
import starling.bouncyrmi.BouncyRMIClient
import starling.props.Props
import starling.auth.Client
import starling.trade.facility.TradeFacility
import starling.fc2.api.FC2Facility
import starling.reports.facility.ReportFacility
import starling.utils.ThreadUtils
import starling.daterange.Day
import starling.utils.Stopwatch
import collection.immutable.TreeSet
import starling.gui.api._
import starling.tradestore.TradePredicate
import starling.pivot._
import java.util.Random

/**
 * We go from 10 clients up to 200 clients running the same report with different layouts. We get the maximum time a client waits for the report
 * and the average time for the report for each step and output the information. Use excel to graph the information and see if we are linear with
 * number of clients.
 */
object TitanAreWeLinearTest {
  def main(args:Array[String]) {
    val props = initialSetupAndReturnProps
    val run = Server.run(props)

    val results = new ListBuffer[(String,String)]()

    val day = Day(2012, 1, 6)
    val stopwatch = Stopwatch()
    val random = new Random(12345L)

    val (tradeFacility, fc2Facility, reportFacility) = createTradeFC2AndReportFacility(props)

    val pricingGroup = PricingGroup.Metals
    val latestMarketDataVersion = fc2Facility.init().pricingGroupLatestMarketDataVersions(pricingGroup)
    val marketDataSelection = MarketDataSelection(Some(pricingGroup), None)
    val marketDataIdentifier = MarketDataIdentifier(marketDataSelection, latestMarketDataVersion)

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
    val desk = Desk.Titan
    val deskCloses = tradeFacility.init().deskCloses(desk)(TradeTimestamp.magicLatestTimestampDay)
    val tradeTimestamp = deskCloses.sortWith(_.timestamp > _.timestamp).head
    val allTradeSelection = TradeSelectionWithTimestamp(Some((desk, tradeTimestamp)), TradePredicate.Null, None)

    val reportPivotFieldState = new PivotFieldsState(
      rowFields = List(Field("Risk Market"), Field("Risk Period")),
      columns = ColumnTrees.dataField(Field("Position"))
    )
    val reportPivotFieldParams = PivotFieldParams(true, Some(reportPivotFieldState))
    val prl = reportFacility.reportOptionsAvailable.options.filter(_.slidable)
    val reportOptions = ReportOptions(prl,None,None)
    val reportParameters = ReportParameters(allTradeSelection, curveIdentifierLabel, reportOptions, day.startOfFinancialYear, None, true)

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
    runReportOnce(reportFacility, reportParameters, reportPivotFieldParams)
    val runReportOnceTime = stopwatch.asStringAndReset

    val twoHundredTradeFC2sAndReportFacilities = (0 until 200).map(_ => createTradeFC2AndReportFacility(props))
    val twoHundredReportFacilities = twoHundredTradeFC2sAndReportFacilities.map(_._3)

    val delta = 10
    var numClients = 10
    while (numClients <= 200) {
      val reportFacilities = twoHundredReportFacilities.take(numClients)
      val readResults = reportFacilities.map(reportFacility => runReportAfterDelay(reportFacility, reportParameters, randomPivotFieldParams, random.nextInt(10000).toLong))
      readResults.map(_.thread.join())
      val maxSingleRead = maxSingleReadTextNoDesc(readResults)
      results += ((numClients.toString + "\t", maxSingleRead))
      numClients += delta
    }


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
      ("", ""),
      ("Run the report once", runReportOnceTime)
    ) ++ results.toList
    val maxLength = output.map(_._1.length()).max + 1
    val maxResultLength = output.map(_._2.length()).max
    val formattedOutput = output.map{case (description, result) => {(description.padTo(maxLength, " ").mkString, result)}}

    println("Results".padTo(maxLength + maxResultLength, "-").mkString)
    formattedOutput.foreach{case (description, result) => println(description + result)}
    println("-".padTo(maxLength + maxResultLength, "-").mkString)
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
}