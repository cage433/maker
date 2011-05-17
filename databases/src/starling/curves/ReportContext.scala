package starling.curves

import starling.daterange.{Day, TimeOfDay, DayAndTime}
import starling.quantity.UOM

/**
 * All reports retrieve their market data via one of these
 */
trait ReportContext {
  def valuationCCY:UOM = UOM.USD
  def marketDay = environment.marketDay.day

  
  def marketDayAndTime:DayAndTime
  def environment:Environment
  def environmentFor(observationDay:ObservationDay):Environment
  def observationDays(from:Day, to:Day):List[ObservationDay]
  def thetaDayAndTime:DayAndTime
}
