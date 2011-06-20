package starling.calendar

import java.lang.String
import starling.utils.Log

object HolidayTablesFactory {
  private var holidayTablesImpl: Option[HolidayTables] = None

  def registerHolidayTablesImpl(holidayTables: HolidayTables) {
    holidayTablesImpl match {
      case None => holidayTablesImpl = Some(holidayTables)
      case Some(registeredHolidayTables) if registeredHolidayTables == holidayTables => {}
      case Some(holidayTable) => throw new Exception("Implementation already registered")
    }
  }

  def registerNewHolidayTablesImplForTesting(holidayTables: Option[HolidayTables]) {
      holidayTablesImpl = holidayTables
  }

  def holidaysTablesOption = holidayTablesImpl

  def holidayTables = {
    holidayTablesImpl match {
      case None => throw new Exception("Implementation not yet registered")
      case Some(holidayTables) => holidayTables
    }
  }
}
