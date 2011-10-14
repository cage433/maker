package starling.startserver.apps

import starling.utils.Stopwatch
import starling.services.StarlingInit
import com.trafigura.services.marketdata.MarketDataServiceApi
import starling.client.BouncyRMIServiceApi
import starling.startserver.SingleClasspathBroadcasterActivator
import starling.auth.osgi.AuthBromptonActivator
import starling.services.osgi.ServicesBromptonActivator
import starling.bouncyrmi.BouncyRMIServerBromptonActivator
import starling.singleclasspathmanager.SingleClasspathManager
import starling.titan.TitanEdmTradeService
import starling.services.rpc.valuation.ValuationService
import starling.services.rpc.marketdata.MarketDataService
import starling.daterange.Day
import starling.utils.ImplicitConversions._
import starling.titan.EDMConversions._
import com.trafigura.services.valuation.{TitanMarketDataIdentifier, ValuationServiceApi}

/**
 * Run up a test instance of the server and invoke the valuation service operations to test services using mock data
 *   for service dependencies
 */
object ValuationServicePerformanceTest extends App {

  MetalsRunner.runWithMockTitan { lookup => {
    import java.io._
    println("Running main for valuation service tests")
    val gSw = new Stopwatch()

    lazy val mds = lookup(classOf[MarketDataService])
    lazy val vs = lookup(classOf[ValuationService])

    println("Took %s to start the test server".format(gSw))

    val runCount = 10

    def run[T](desc : String, f : () => T) : List[(Int, Long, T)] = {
      (1 to runCount).map {n =>
        val sw = new Stopwatch()
        val valuations = f()
        println("Call for %s, run %d took %s".format(desc, n, sw.toString()))
        (n, sw.ms(), valuations)
      }.toList
    }

    def showResults[T <: Either[_, _]](ls : List[T], desc : String) = {
      val (worked, errors) = ls.partition(_.isRight)

      println("\nCalled valueAll for %s, %d worked, %d failed, took %s".format(desc, worked.size, errors.size, gSw))

      //println("\nSuccessful %s valuations:\n".format(desc))
      //worked.foreach(println)

      //println("\nFailed %s valuations:\n".format(desc))
      //errors.foreach(println)
    }

    BouncyRMIServiceApi().using { valuationServiceRMI : ValuationServiceApi =>
      val snapshotID = mds.marketDataSnapshotIDs(Some(Day.today)).head
      val marketDataID = TitanMarketDataIdentifier(snapshotID, Day.today)
      val directQuotaResults = run("Quota (direct)", () => vs.valueAllTradeQuotas(marketDataID))
      val rmiQuotaResults = run("Quota (rmi)", () => valuationServiceRMI.valueAllTradeQuotas(marketDataID))

      val output = new File("valuation-service-timings.csv")
      val w = new PrintWriter(output)

      def printHeaders() = w.println("Run number, time (ms), total record count, successfully valued record count\n")

      printHeaders()
      w.println(rmiQuotaResults.map(r => List(r._1, r._2, r._3.values.size, r._3.values.partition(_.isRight)._1.size).mkString(", ")).mkString("\n"))
      w.println("\nDirect quota results (average time = %dms)".format(average(directQuotaResults.map(_._2).toList)))
      printHeaders()
      w.println(directQuotaResults.map(r => List(r._1, r._2, r._3.values.size, r._3.values.partition(_.isRight)._1.size).mkString(", ")).mkString("\n"))

      printHeaders()
      w.println(rmiQuotaResults.map(r => List(r._1, r._2, r._3.values.size, r._3.values.partition(_.isRight)._1.size).mkString(", ")).mkString("\n"))
      printHeaders()

      w.flush
      w.close
    }

    BouncyRMIServiceApi().using { marketDataRMI : MarketDataServiceApi =>
      val latestSnapshot = mds.latestSnapshotID().getOrThrow("No snapshots")
      val today = Day.today
      val latestMarketDataID = TitanMarketDataIdentifier(latestSnapshot, today)

      val rmiSpotFXResults = run("Spot FX (rmi)", () => mds.getSpotFXRates(latestMarketDataID))
      val rmiReferenceInterestRatesResults = run("Reference interest rates (rmi)", () => mds.getReferenceInterestRates(latestMarketDataID))
      val rmiSnapshotIds = run("Reference interest rates (rmi)", () => mds.marketDataSnapshotIDs())

      val output = new File("marketdata-service-timings.csv")
      val w = new PrintWriter(output)

      def printHeaders() = w.println("Run no, time (ms), total record count\n")

      w.println("\nRMI spotfx results (average time = %dms)".format(average(rmiSpotFXResults.map(_._2).toList)))
      printHeaders()
      w.println(rmiSpotFXResults.map(r => List(r._1, r._2, r._3.size).mkString(", ")).mkString("\n"))

      w.println("\nRMI reference data interest rate results (average time = %dms)".format(average(rmiReferenceInterestRatesResults.map(_._2).toList)))
      printHeaders()
      w.println(rmiReferenceInterestRatesResults.map(r => List(r._1, r._2, r._3.size).mkString(", ")).mkString("\n"))

      w.println("\nRMI snapshot id results (average time = %dms)".format(average(rmiSnapshotIds.map(_._2).toList)))
      printHeaders()
      w.println(rmiSnapshotIds.map(r => List(r._1, r._2, r._3.size).mkString(", ")).mkString("\n"))

      w.flush
      w.close
    }
  } }
  //    val quotaValuations = valuationServiceRMI.valueAllQuotas()
  //    showResults(quotaValuations.tradeResults.values.toList, "Quotas")

  //    val inventoryValuations = vs.valueAllInventory()
  //    showResults(inventoryValuations.assignmentValuationResults.values.toList, "Inventory")

    def average(ls : List[Long]) = ls.sum/ls.size
}
