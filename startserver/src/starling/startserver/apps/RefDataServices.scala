package starling.startserver.apps

import starling.services.StarlingInit
import starling.services.rpc.refdata.FileMockedTitanServicesDataFileGenerator._
import starling.services.rpc.refdata.FileMockedTitanServicesDataFileGenerator
import starling.startserver.SingleClasspathBroadcasterActivator
import starling.auth.osgi.AuthBromptonActivator
import starling.services.osgi.ServicesBromptonActivator
import starling.reports.osgi.ReportsBromptonActivator
import starling.singleclasspathmanager.SingleClasspathManager
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