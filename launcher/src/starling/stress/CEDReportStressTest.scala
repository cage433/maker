package starling.stress

import starling.launcher.DevLauncher
import starling.props.Props
import starling.http.GUICode
import starling.api.utils.PropertyValue
import starling.startserver.Server
import starling.daterange.Day
import java.util.Random
import collection.mutable.ListBuffer
import starling.bouncyrmi.{BouncyRMIClient, BouncyRMI}
import starling.auth.Client
import starling.trade.facility.TradeFacility
import starling.fc2.api.FC2Facility
import starling.reports.facility.ReportFacility
import starling.utils.{ThreadUtils, Stopwatch}
import collection.immutable.TreeSet
import starling.gui.api._
import starling.tradestore.TradePredicate
import starling.manager.{TimeTree, Profiler}
import starling.pivot._

object CEDReportStressTest {
  def initialSetupAndReturnProps = {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    val props0 = DevLauncher.propsWithUnusedPort()

    val source = "stress"
    val propsMap = props0.starlingProps +
      ("ImportMarketDataAutomatically" -> PropertyValue(source, "false")) +
      ("ServerType" -> PropertyValue(source, "Oil")) +
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

    // For CED there are a lot of trades. If we include this with the possible row area fields it can get very slow and run out of memory.
    val useTradeID = false
//    val useTradeID = true

    val day = Day(2012, 1, 10)
    val stopwatch = Stopwatch()
    val random = new Random(12345L)
    val results = new ListBuffer[(String,String)]()

    val (tradeFacility, fc2Facility, reportFacility) = createTradeFC2AndReportFacility(props)

    val pricingGroup = PricingGroup.System
    val latestMarketDataVersion = fc2Facility.init().pricingGroupLatestMarketDataVersions(pricingGroup)
    val marketDataSelection = MarketDataSelection(Some(pricingGroup), None)
    val marketDataIdentifier = MarketDataIdentifier(marketDataSelection, latestMarketDataVersion)

    val mods = TreeSet[EnvironmentModifierLabel]()
    val observationDay = day.endOfDay
    val curveIdentifierLabel = CurveIdentifierLabel(
      marketDataIdentifier,
      EnvironmentRuleLabel.COB,
      observationDay,
      observationDay,
      observationDay.day.nextWeekday.endOfDay,
      mods
    )

    val desk = Desk.CED
    val deskCloses = tradeFacility.init().deskCloses(desk)
    val tradeTimestamp = deskCloses(deskCloses.keys.max).max
    val allTradeSelection = TradeSelectionWithTimestamp(Some((desk, tradeTimestamp)), TradePredicate.Null, None)

    val reportPivotFieldState1 = new PivotFieldsState(
      rowFields = List(Field("Risk Market"), Field("Risk Period")),
      columns = ColumnTrees.dataField(Field("Position"))
    )
    val reportPivotFieldParams1 = PivotFieldParams(true, Some(reportPivotFieldState1))
    val prl = reportFacility.reportOptionsAvailable.options.filter(_.slidable)
    val reportOptions = ReportOptions(prl,None,None)

    // We want to use a trade expiry day of 1Oct2010 so we get as many trades as possible.
    val tradeExpiryDay = Day(2010,10,1)

    val reportParameters = ReportParameters(allTradeSelection, curveIdentifierLabel, reportOptions, tradeExpiryDay, None, true)

    val reportPivotFieldState2 = new PivotFieldsState(
      rowFields = List(Field("Risk Period"), Field("Risk Market")),
      columns = ColumnTrees.dataField(Field("Position"))
    )
    val reportPivotFieldParams2 = PivotFieldParams(true, Some(reportPivotFieldState2))

    stopwatch.reset()
    runReportOnce(reportFacility, reportParameters, reportPivotFieldParams1)
    val client1RunningReportFor1stTime = stopwatch.asStringAndReset
    runReportOnce(reportFacility, reportParameters, reportPivotFieldParams1)
    val client1RunningReportFor2ndTime = stopwatch.asStringAndReset
    runReportOnce(reportFacility, reportParameters, reportPivotFieldParams2)
    val client1RunningReportFor3rdTimeDifferentLayout = stopwatch.asStringAndReset

    val potentialRowFields = List(Field("Risk Market"), Field("Risk Period"), Field("Instrument"), Field("Risk Type"), Field("Risk Commodity")) ::: (if (useTradeID) List(Field("Trade ID")) else Nil)
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

    val tenTradeFC2sAndReportFacilities = (0 until 10).map(_ => createTradeFC2AndReportFacility(props))
    val tenReportFacilities = tenTradeFC2sAndReportFacilities.map(_._3)

    stopwatch.reset()
    val tenClientsResults = tenReportFacilities.map(reportFacility => {
      runReportAfterDelay(reportFacility, reportParameters, randomPivotFieldParams, random.nextInt(10000).toLong)})
    tenClientsResults.map(_.thread.join())
    val tenClientsRunningSameReportDifferentLayouts = stopwatch.asStringAndReset
    val maxSingleReadFor10ClientsRunningSameReportDifferentLayout = maxSingleReadText(tenClientsResults)


    val twoHundredTradeFC2sAndReportFacilities = (0 until 200).map(_ => createTradeFC2AndReportFacility(props))
    val twoHundredReportFacilities = twoHundredTradeFC2sAndReportFacilities.map(_._3)

    stopwatch.reset()
    val twoHundredClientsResults = twoHundredReportFacilities.map(reportFacility => {
          runReportAfterDelay(reportFacility, reportParameters, randomPivotFieldParams, random.nextInt(10000).toLong)})
    twoHundredClientsResults.map(_.thread.join())
    val twoHundredClientsRunningSameReportDifferentLayouts = stopwatch.asStringAndReset
    val maxSingleReadFor200ClientsRunningSameReportDifferentLayout = maxSingleReadText(twoHundredClientsResults)


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
      ("Client 1 running report for 1st time:", client1RunningReportFor1stTime),
      ("Client 1 running report for 2nd time:", client1RunningReportFor2ndTime),
      ("Client 1 running report for 3rd time with different layout:", client1RunningReportFor3rdTimeDifferentLayout),
      ("10 clients running same report, different layout with random delay up to 10 seconds:", tenClientsRunningSameReportDifferentLayouts),
      ("10 clients running same report, different layout with random delay up to 10 seconds max single read:", maxSingleReadFor10ClientsRunningSameReportDifferentLayout),
//      ("200 clients running same report, different layout with random delay up to 10 seconds:", twoHundredClientsRunningSameReportDifferentLayouts),
//      ("200 clients running same report, different layout with random delay up to 10 seconds max single read:", maxSingleReadFor200ClientsRunningSameReportDifferentLayout),
      ("", "")
    ) ++ results.toList
    val maxLength = output.map(_._1.length()).max + 1
    val maxResultLength = output.map(_._2.length()).max
    val formattedOutput = output.map{case (description, result) => {(description.padTo(maxLength, " ").mkString, result)}}

    println("Results".padTo(maxLength + maxResultLength, "-").mkString)
    formattedOutput.foreach{case (description, result) => println(description + result)}
    println("-".padTo(maxLength + maxResultLength, "-").mkString)
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
}