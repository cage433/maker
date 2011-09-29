package starling.gui

import starling.rmi.{StarlingServer}
import starling.browser.{ServerContext, SubmitRequest}
import starling.reports.facility.ReportFacility
import starling.trade.facility.TradeFacility

trait StarlingSubmitRequest[R] extends SubmitRequest[R] {
  def baseSubmit(serverContext:ServerContext) = {
    submit(new StarlingServerContext(
      serverContext.lookup(classOf[StarlingServer]),
      serverContext.lookup(classOf[ReportFacility]),
      serverContext.lookup(classOf[TradeFacility])
    ))
  }
  def submit(server:StarlingServerContext):R
}

class StarlingServerContext(
    val server:StarlingServer,
    val reportService:ReportFacility,
    val tradeService:TradeFacility)


