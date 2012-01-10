package starling.launcher

import collection.mutable.ListBuffer
import starling.bouncyrmi.BouncyRMIClient
import starling.props.Props
import starling.auth.Client
import starling.trade.facility.TradeFacility
import starling.fc2.api.FC2Facility
import starling.reports.facility.ReportFacility
import starling.startserver.Server
import starling.daterange.Day
import starling.utils.{ThreadUtils, Stopwatch}
import starling.pivot._
import starling.gui.api._
import StressTest._
import java.util.Random

/**
 * Each client requests the same market data at a random point inside a 10 second window. The number of clients increases until the time it takes
 * a single client to receive the market data is greater than 10 seconds.
 */
object BreakingStressTest {
  def main(args:Array[String]) {

    val largeDataSet = true

    val props = initialSetupAndReturnProps
    val run = Server.run(props)

    val results = new ListBuffer[(String,String)]()
    var firstReadOfPriceData = ""
    var secondReadOfPriceData = ""
    var thirdReadOfPriceData = ""
    var twoHundredClientsReadingAfterDelay = ""
    var maximumReadTimeForTwoHundredClientsWithDelay = ""
    var maximumReadTimeForTwoHundredClientsWithDelayTree = ""

    try {

      val day = Day(2012, 1, 6)
      val stopwatch = Stopwatch()

      val (tradeFacility1, fc2Facility1, reportFacility1) = createTradeFC2AndReportFacility(props)

      val pricingGroup = PricingGroup.Metals
      val latestMarketDataVersion = fc2Facility1.init().pricingGroupLatestMarketDataVersions(pricingGroup)
      val marketDataSelection = MarketDataSelection(Some(pricingGroup), None)
      val marketDataIdentifier = MarketDataIdentifier(marketDataSelection, latestMarketDataVersion)
      val marketDataPageIdentifier = StandardMarketDataPageIdentifier(marketDataIdentifier)

      val filters = if (largeDataSet) {
        List((Field("Observation Day"), SomeSelection(Set(day))))
      } else {
        List((Field("Observation Day"), SomeSelection(Set(day))), (Field("Market"), SomeSelection(Set("COMEX Gold"))))
      }
      val pricePivotFieldsState01 = new PivotFieldsState(
        filters = filters,
        rowFields = List(Field("Market"), Field("Observation Time"), Field("Period")),
        columns = ColumnTrees.dataField(Field("Price")))
      val pricePivotFieldParams01 = PivotFieldParams(true, Some(pricePivotFieldsState01))
      val priceMarketDataLabel = MarketDataTypeLabel("Price")

      stopwatch.reset()
      readMarketDataOnce(fc2Facility1, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01)
      firstReadOfPriceData = stopwatch.asStringAndReset
      readMarketDataOnce(fc2Facility1, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01)
      secondReadOfPriceData = stopwatch.asStringAndReset
      readMarketDataOnce(fc2Facility1, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01)
      thirdReadOfPriceData = stopwatch.asStringAndReset

      val twoHundredTradeFC2sAndReportFacilities = (0 until 200).map(_ => createTradeFC2AndReportFacility(props))
      val twoHundredFC2s = twoHundredTradeFC2sAndReportFacilities.map(_._2)
      val random = new Random(12345L)

      stopwatch.reset()
      val twoHundredReadingAfterDelayResults = twoHundredFC2s.map(fc2 => readMarketDataAfterDelay(fc2, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01, random.nextInt(10000).toLong))
      twoHundredReadingAfterDelayResults.foreach(_.thread.join())
      twoHundredClientsReadingAfterDelay = stopwatch.asStringAndReset
      var maxResult = twoHundredReadingAfterDelayResults.maxBy(_.maxMillis())
      maximumReadTimeForTwoHundredClientsWithDelay = Stopwatch.milliToHumanString(maxResult.maxMillis())
      maximumReadTimeForTwoHundredClientsWithDelayTree = maxResult.timeTree().text()

      val fc2s = new ListBuffer[FC2Facility]()
      fc2s ++= twoHundredFC2s
      while (maxResult.maxMillis() < 10000) {
        val extra50FC2s = (0 until 50).map(_ => createTradeFC2AndReportFacility(props)).map(_._2)
        fc2s ++= extra50FC2s
        stopwatch.reset()
        val readResults = fc2s.map(fc2 => readMarketDataAfterDelay(fc2, marketDataPageIdentifier, priceMarketDataLabel, pricePivotFieldParams01, random.nextInt(10000).toLong))
        readResults.foreach(_.thread.join())
        val timeTaken = stopwatch.asStringAndReset
        maxResult = readResults.maxBy(_.maxMillis())
        results += ((fc2s.size + " clients reading price data with random delay up to 10 seconds:", timeTaken))
        results += ((fc2s.size + " clients reading price data with random delay up to 10 seconds max single read:", Stopwatch.milliToHumanString(maxResult.maxMillis())))
        println("*** Up to " + clients.size + " clients ***")
      }
    } catch {
      case t => {
        t.printStackTrace()
        println("")
        println("")
        println("")
        println("")
        println("Number of clients when we got an exception: " + clients.size)
      }
    } finally {
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
        ("1st client 1st read of price data:", firstReadOfPriceData),
        ("1st client 2nd read of price data:", secondReadOfPriceData),
        ("1st client 3rd first read of price data:", thirdReadOfPriceData),
        ("200 clients reading price data with random delay up to 10 seconds:", twoHundredClientsReadingAfterDelay),
        ("200 clients reading price data with random delay up to 10 seconds max single read:", maximumReadTimeForTwoHundredClientsWithDelay)
      ) ++ results.toList
      val maxLength = output.map(_._1.length()).max + 1
      val maxResultLength = output.map(_._2.length()).max
      val formattedOutput = output.map{case (description, result) => {(description.padTo(maxLength, " ").mkString, result)}}

      println("Results".padTo(maxLength + maxResultLength, "-").mkString)
      formattedOutput.foreach{case (description, result) => println(description + result)}
      println("-".padTo(maxLength + maxResultLength, "-").mkString)
    }
  }

  val clients = new ListBuffer[BouncyRMIClient]()

  def createClient(props:Props) = {
    val client = new BouncyRMIClient("localhost", props.RmiPort(), Client.Null)
    client.startBlocking()
    clients += client
    client
  }

  def createTradeFC2AndReportFacility(props:Props) = {
    val client = createClient(props)
    (client.proxy(classOf[TradeFacility]), client.proxy(classOf[FC2Facility]), client.proxy(classOf[ReportFacility]))
  }
}