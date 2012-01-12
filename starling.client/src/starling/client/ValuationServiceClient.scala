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
      println("Calling marketDataSnapshotIDs...")

      val observationDay = None // Some(Day.today)
      val snapshotIdentifiers = marketDataService.marketDataSnapshotIDs(observationDay)
      snapshotIdentifiers.foreach(println)

      val latestSnapshotIdentifier = snapshotIdentifiers.head
      println("using snapshotId " + latestSnapshotIdentifier)

      def testQuotaValuation() {
        println("Calling valueAllQuotas...")
        val sw = new Stopwatch()

        val (_, valuationResult) = valuationService.valueAllTradeQuotas(None)
        //val (_, valuationResult) = valuationService.valueAllTradeQuotas(Some(TitanMarketDataIdentifier(latestSnapshotIdentifier, Day.today, Day.today)))

        println("Took " + sw)

        val (worked, errors) = valuationResult.values.partition(_.isRight)
        val quotaValuations : List[QuotaValuation] = worked.collect{case Right(qv) => qv}.flatten.toList  // .flatMap(_.right.get).toList

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

        println("Worked trades " + worked.size + ", failed trades " + errors.size)
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

        println("\n\nWorked, details " + worked.mkString("\n"))

        println("Valuation service result, valued quotas, total size %d, worked %d, failed %d, using snapshot %s, took %d ms".format(valuationResult.size, worked.size, errors.size, snapshotIdentifiers.head, sw))
      }

      testQuotaValuation()
    }
  }
}
