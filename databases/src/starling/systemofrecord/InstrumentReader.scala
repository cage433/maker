package starling.systemofrecord

import starling.richdb.RichResultSetRow
import starling.instrument.{Tradeable, Instrument}

/**
 * New instruments should have a reader that extends this class. This means we can
 * create the instrument from a result set.
 */
trait InstrumentReader {
  /**
   * Override this method and return true if the reader can handle this row
   */
  def canHandle(rs : RichResultSetRow) : Boolean

  /**
   * Given the result set create the instrument. If there aren't enough fields available in the result
   * set then add any extra ones to SystemOfRecord.query
   * This is only called if canHandle returned true
   */
  def create(rs : RichResultSetRow) : Tradeable
}