package starling.calendar

import starling.daterange.Day._
import java.lang.String
import starling.daterange.{Location, Day}
import starling.daterange.Location._

trait HolidayTables{
  def regionHolidays(name: String) : BusinessCalendarSet
  def financialHolidays(name: String) : BusinessCalendarSet

  lazy val AT : BusinessCalendarSet = regionHolidays("AT")
  val UK : BusinessCalendarSet = regionHolidays("UK")
  val US : BusinessCalendarSet = regionHolidays("US")

  // These codes come from looking up EAI.dbo.tblFCCalendars. It's manual job of looking up the name
  // of the exchange then finding the correct calendar ID. There's lots of near matches so it can't be automated.
  val LME : BusinessCalendarSet = financialHolidays("LME").copy(location = London)
  val SFE: BusinessCalendarSet = financialHolidays("SFS").copy(location = Shanghai)
  val NYMEX: BusinessCalendarSet = financialHolidays("NYM")
  val ICE: BusinessCalendarSet = financialHolidays("IPE")
  val BALTIC: BusinessCalendarSet = financialHolidays("IcS")

  // Bursa Malaysia trading holidays
  val KLS: BusinessCalendarSet = financialHolidays("KLS")

  // Hack for LBMA as we don't have old holiday data before apr 2nd
  val LBMA: BusinessCalendarSet = BusinessCalendarSet("LBMA", London, financialHolidays("LBM").days ++ UK.days.filter(_ < (2 Apr 2010)))
}

object NullHolidays extends HolidayTables{
  def financialHolidays(name: String) = BusinessCalendarSet(name, Unknown, Set[Day]())
  def regionHolidays(name: String) = BusinessCalendarSet(name, Unknown, Set[Day]())
}
