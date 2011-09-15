package starling.client

import com.trafigura.services.valuation.ValuationServiceApi


/**
 * Test main to check service operation and RMI etc
 */
object ValuationServiceApiTestRun {
  def main(args:Array[String]) {
    BouncyRMIServiceApi().using { valuationService: ValuationServiceApi =>
      println("Calling marketDataSnapshotIDs...")
      val snapshots = valuationService.marketDataSnapshotIDs(None)
      snapshots.foreach(println)

      def testQuotaValuation() {
        println("Calling valueAllQuotas...")
        val sw = System.currentTimeMillis()
        val valuationResult = valuationService.valueAllQuotas(None) // (snapshots.map(_.id).headOption)
        val (worked, errors) = valuationResult.tradeResults.values.partition(_ isRight)
        println("Worked " + worked.size + ", failed " + errors.size)
        println("Errors: ")
        errors.foreach(println)
        println("Valuation service result, valued quotas, size %d, using snapshot %s, took %d ms".format(valuationResult.tradeResults.size, valuationResult.snapshotID, System.currentTimeMillis() - sw))

        val errs = errors.collect{ case Left(e) => e }.filter(e => !e.contains("Fixed pricing spec with no fixed prices")).filter(!_.contains("SHFE, Nickel"))

        val (missingMarkets, other) = errs.partition(_.contains("No market"))
        val (missingLevels, other2) = other.partition(_.contains("only have levels"))
        println("\nFiltered errors = \n" + errs.mkString("\n"))
        println("\nMissing Markets " + missingMarkets.mkString("\n"))
        println("\nMissing Levels " + missingLevels.mkString("\n"))
        println("\nMisc " + other2.mkString("\n"))
      }

      def testAssignmentValuation() {
        println("Calling valueAllInventory...")
        val sw = System.currentTimeMillis()
        val assignmentValuationResult = valuationService.valueAllInventory(None)
        val (worked, errors) = assignmentValuationResult.assignmentValuationResults.values.partition(_ isRight)
        println("Worked " + worked.size + ", failed " + errors.size)
        println("Errors: ")
        errors.foreach(println)
        println("Valuation service result, valued assignment size %d, using snapshot %s, took %d ms".format(assignmentValuationResult.assignmentValuationResults.size, assignmentValuationResult.snapshotID, System.currentTimeMillis() - sw))

        val errs = errors.collect{ case Left(e) => e }.filter(e => !e.contains("Fixed pricing spec with no fixed prices")).filter(!_.contains("SHFE, Nickel"))

        val (missingMarkets, other) = errs.partition(_.contains("No market"))
        val (missingLevels, other2) = other.partition(_.contains("only have levels"))
        println("\nFiltered errors = \n" + errs.mkString("\n"))
        println("\nMissing Markets " + missingMarkets.mkString("\n"))
        println("\nMissing Levels " + missingLevels.mkString("\n"))
        println("\nMisc " + other2.mkString("\n"))
      }

      testQuotaValuation()
      testAssignmentValuation()
    }
  }
}
