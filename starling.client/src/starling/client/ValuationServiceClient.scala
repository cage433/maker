package starling.client

import com.trafigura.services.valuation.ValuationServiceApi


/**
 * Test main to check service operation and RMI etc
 */
object ValuationServiceClient {
  def main(args:Array[String]) {
    BouncyRMIServiceApi().using { valuationService: ValuationServiceApi =>
      println("Calling marketDataSnapshotIDs...")
      val snapshots = valuationService.marketDataSnapshotIDs(None)

      snapshots.foreach(println)

      //valuationService.valueAllTradeQuotas(Some("4622"))

      def testQuotaValuation() {
        println("Calling valueAllQuotas...")
        val sw = System.currentTimeMillis()

        //val x = valuationService.valueSingleTradeQuotas("306")

        val valuationResult = valuationService.valueAllTradeQuotas(None) // (snapshots.map(_.id).headOption)
        val (worked, errors) = valuationResult.valuationResults.values.partition(_ isRight)
        println("Worked " + worked.size + ", failed " + errors.size)
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

        println("Valuation service result, valued quotas, total size %d, worked %d, failed %d, using snapshot %s, took %d ms".format(valuationResult.valuationResults.size, worked.size, errors.size, valuationResult.snapshotID, System.currentTimeMillis() - sw))
      }

      def testAssignmentValuation() {
        println("Calling valueAllInventory...")
        val sw = System.currentTimeMillis()
        val assignmentValuationResult = valuationService.valueAllInventory(None)
        val (worked, errors) = assignmentValuationResult.assignmentValuationResults.values.partition(_ isRight)
        println("Worked " + worked.size + ", failed " + errors.size)
        println("Errors: ")
        errors.foreach(println)

        val errs = errors.collect{ case Left(e) => e }.filter(e => !e.contains("Fixed pricing spec with no fixed prices")).filter(!_.contains("SHFE, Nickel"))

        val (missingMarkets, other) = errs.partition(_.contains("No market"))
        val (missingLevels, other2) = other.partition(_.contains("only have levels"))
        println("\nFiltered errors = \n" + errs.mkString("\n"))
        println("\nMissing Markets " + missingMarkets.mkString("\n"))
        println("\nMissing Levels " + missingLevels.mkString("\n"))
        println("\nMisc " + other2.mkString("\n"))

        println("Valuation service result, valued assignment total size %d, worked %d, failed %d, using snapshot %s, took %d ms".format(assignmentValuationResult.assignmentValuationResults.size, worked.size, errors.size, assignmentValuationResult.snapshotID, System.currentTimeMillis() - sw))
      }

      testQuotaValuation()
      testAssignmentValuation()
    }
  }
}
