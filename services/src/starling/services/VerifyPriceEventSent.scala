package starling.services

import starling.daterange.Day
import starling.utils.Broadcaster
import starling.utils.ImplicitConversions._
import starling.databases.MarketDataEventSource
import starling.gui.api.Email


case class VerifyPriceEventSent(marketDataAvailability: MarketDataAvailabilityChecker, broadcaster: Broadcaster,
                                dataFlow: DataFlow with MarketDataEventSource) {
//  extends EmailingScheduledTask(broadcaster, dataFlow.from, dataFlow.to) with Log {

//  override def attributes = super.attributes +
//    (DataSource → ScheduledTaskAttribute(dataFlow.source)) + (DataSink → ScheduledTaskAttribute(dataFlow.sink))

  def emailFor(observationDay: Day, email: Email): Option[Email] = if (marketDataAvailability.dataAvailable(dataFlow)) {
    marketDataAvailability.await(dataFlow)
    None
  } else {
    Some(email.copy(subject = "Missing Prices for: %s on %s" % (dataFlow.description, observationDay.toString),
                    body = <p>No Prices have been uploaded for {dataFlow.description} on {observationDay.toString}</p>.toString))
  }
}