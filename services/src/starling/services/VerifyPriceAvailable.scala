package starling.services

import starling.daterange.Day
import starling.utils.{Log, Broadcaster}
import starling.gui.api.EmailEvent

import starling.utils.ImplicitConversions._


case class VerifyPriceAvailable(marketDataAvailability: MarketDataAvailabilityChecker, broadcaster: Broadcaster,
                                dataFlow: DataFlow with MarketDataEventSource)
  extends EmailingScheduledTask(broadcaster, dataFlow.from, dataFlow.to) with Log {

  override def attributes = super.attributes +
    (DataSource → ScheduledTaskAttribute(dataFlow.source)) + (DataSink → ScheduledTaskAttribute(dataFlow.sink))

  def eventFor(observationDay: Day, email: EmailEvent): Option[EmailEvent] = if (marketDataAvailability.dataAvailable(dataFlow)) {
    marketDataAvailability.await(dataFlow)
    None
  } else {
    Some(email.copy(subject = "Missing Prices for: %s on %s" % (dataFlow.description, observationDay),
                    body = <p>No Prices have been uploaded for {dataFlow.description} on {observationDay}</p>.toString))
  }
}