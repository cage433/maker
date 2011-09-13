package starling.market

import starling.curves.InstrumentLevelEnvironment
import starling.daterange.{DateRange, Day}
import starling.quantity.{UOM, Quantity}

trait InstrumentLevelKnownPrice {
  /**
   * forwardDate will be None for Indexes as they know the rule for calculating the forward date from the observationDay
   */
  def fixing(env : InstrumentLevelEnvironment, observationDay : Day, forwardDate: Option[DateRange]): Quantity

  /**
   * period will be either:
   *  observation day
   *  or
   *  forward date
   */
  def forwardPrice(env : InstrumentLevelEnvironment, period: Either[Day, DateRange], ignoreShiftsIfPermitted: Boolean): Quantity = period match {
    case Left(observationDay: Day) => forwardPriceOnObservationDay(env, observationDay, ignoreShiftsIfPermitted)
    case Right(forwardDate: DateRange) => forwardPriceForPeriod(env, forwardDate, ignoreShiftsIfPermitted)
  }

  def forwardPriceOnObservationDay(env : InstrumentLevelEnvironment, observationDay: Day, ignoreShiftsIfPermitted: Boolean): Quantity = throw new Exception("Not implemented for " + (this, this.getClass))

  def forwardPriceForPeriod(env : InstrumentLevelEnvironment, period: DateRange, ignoreShiftsIfPermitted: Boolean): Quantity = throw new Exception("Not implemented for " + (this, this.getClass))

  def priceUOM: UOM
}