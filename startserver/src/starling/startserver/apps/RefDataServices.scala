package starling.startserver.apps

import starling.services.rpc.refdata.FileMockedTitanServicesDataFileGenerator
import starling.titan.TitanEdmTradeService
import starling.services.rpc.valuation.ValuationService


object RefDataServices {
  def main(args : Array[String]) {
    println("running main for tactical ref data services")
    MetalsRunner.runWithMetalsBundles { lookup =>
      val edmTradeService = lookup(classOf[TitanEdmTradeService])
      val valuationService = lookup(classOf[ValuationService])
      FileMockedTitanServicesDataFileGenerator(edmTradeService, valuationService)
    }
  }

}