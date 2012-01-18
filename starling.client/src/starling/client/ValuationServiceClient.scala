package starling.client

import starling.daterange.Day
import com.trafigura.services.valuation.{TitanMarketDataIdentifier, ValuationServiceApi}
import com.trafigura.services.marketdata.MarketDataServiceApi
import starling.titan.valuation._
import starling.utils.Stopwatch


/**
 * Test main to check service operation and RMI etc
 */
object ValuationServiceClient {
  def main(args:Array[String]) = BouncyRMIServiceApi().using { marketDataService: MarketDataServiceApi =>
    BouncyRMIServiceApi().using { valuationService: ValuationServiceApi =>
      try {
        val noOfRuns = 10
        println("Calling marketDataSnapshotIDs...")

        val valFn = () => valuationService.valueAllTradeQuotas(None)
        //val (_, valuationResult) = valuationService.valueAllTradeQuotas(Some(TitanMarketDataIdentifier(latestSnapshotIdentifier, Day.today, Day.today)))

        val observationDay = None // Some(Day.today)
        val snapshotIdentifiers = marketDataService.marketDataSnapshotIDs(observationDay)
        //snapshotIdentifiers.foreach(println)
        val latestSnapshotIdentifier = snapshotIdentifiers.head
        println("latest snapshotId " + latestSnapshotIdentifier)

        def testQuotaValuation() {
          println("\nCalling valueAllQuotas...")
          val sw = new Stopwatch()
          val result = valFn()
          println("made call ok, took " + sw)
          val (snapshot, valuationResult) = result

          stats(snapshot, valuationResult, sw)

          val times = (1 to noOfRuns).map(n => {
            println("\nRun %d Calling valueAllQuotas...".format(n))
            sw.reset
            val (_, valuationResult) = valFn()
            println("Run %d Took %s".format(n, sw))
            val (worked, errors) = valuationResult.values.partition(_.isRight)
            println("Run %d Worked trades %d, failed trades %d".format(n, worked.size, errors.size))
            sw.ms()
          })

          println("average time over %d runs, %dms".format(noOfRuns, times.sum / times.size))
        }

        def stats(snapshot : TitanMarketDataIdentifier, valuationResult : Map[String, Either[String, List[QuotaValuation]]], sw : Stopwatch) {
          println("trade quota valuation selected " + snapshot)
          val (worked, errors) = valuationResult.values.partition(_.isRight)
          val quotaValuations : List[QuotaValuation] = worked.collect{case Right(qv) => qv}.flatten.toList

          val pAssignmentValuations : List[CostsAndIncomeAllocatedPurchaseAssignmentValuation] = quotaValuations.flatMap{
            case pqv : PurchaseQuotaValuation => pqv.assignmentValuations.values
            case _ => Nil
          }

          val sAssignmentValuations : List[CostsAndIncomeAllocatedSaleAssignmentValuation] = quotaValuations.flatMap{
            case sqv : SalesQuotaValuation => sqv.assignmentValuations.values
            case _ => Nil
          }

          val assignmentValuationsCount = pAssignmentValuations.size + sAssignmentValuations.size

          val unassignedValuations : List[PricingValuationDetails] = quotaValuations.flatMap{
            case sqv : SalesQuotaValuation => sqv.unallocatedValuationDetails match {
              case Right(uv) => uv
              case _ => Nil
            }
            case _ => Nil
          }.toList

          println("Worked quotaValuations " + quotaValuations.size + ", assignmentValuations "  + assignmentValuationsCount + ", unassignedValuations " + unassignedValuations.size)

          println("Errors: ")
          errors.foreach(println)

          val errs = errors.collect{ case Left(e) => e }.filter(e => !e.contains("Fixed pricing spec with no fixed prices")).filter(!_.contains("SHFE, Nickel"))

          val (missingMarkets, other) = errs.partition(_.contains("No market"))
          val (missingLevels, other2) = other.partition(_.contains("only have levels"))
          println("\nFiltered errors = \n" + errs.mkString("\n"))
          println("\nMissing Markets " + missingMarkets.mkString("\n"))
          println("\nMissing Levels " + missingLevels.mkString("\n"))
          println("\nMisc " + other2.mkString("\n"))

          //println("\n\nWorked, details " + worked.mkString("\n"))

          println("Valuation service result, valued quotas, total size %d, worked %d, failed %d, using snapshot %s, took %s".format(valuationResult.size, worked.size, errors.size, snapshotIdentifiers.head, sw))
        }

        testQuotaValuation()
      }
      catch {
        case e => println("Caught " + e)
      }
    }
  }
}
