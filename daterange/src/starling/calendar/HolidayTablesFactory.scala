package starling.calendar

object HolidayTablesFactory {

  private var holidayTablesImpl: Option[HolidayTables] = None
  private var previousRegistrant: Exception = _

  def registerHolidayTablesImpl(holidayTables: HolidayTables) {
    holidayTablesImpl match {
      case None => registerNewHolidayTablesImplForTesting(Some(holidayTables))
      case Some(registeredHolidayTables) if registeredHolidayTables == holidayTables => {}
      case Some(holidayTable) => throw new Exception("Implementation already registered", previousRegistrant)
    }
  }

  def registerNewHolidayTablesImplForTesting(holidayTables: Option[HolidayTables]) {
    holidayTablesImpl = holidayTables
    previousRegistrant = new Exception("Previous registrant")
  }

  def holidaysTablesOption = holidayTablesImpl

  def holidayTables = {
    holidayTablesImpl match {
      case None => throw new Exception("Implementation not yet registered")
      case Some(holidayTables) => holidayTables
    }
  }
}
