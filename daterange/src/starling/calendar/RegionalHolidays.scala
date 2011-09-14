package starling.calendar

import java.lang.String
import starling.utils.StringIO
import starling.daterange.{Location, Day}

trait RegionalHolidays extends HolidayTables {

  private def days(name: String) = {
    val resource = "/starling/calendar/" + name
    val days = StringIO.readStringFromResource(classOf[RegionalHolidays], resource).split('\n').map(Day.parse).toSet
    days
  }

  def regionHolidays(name: String) = {
    name match {
      case "UK.txt" | "US.txt" => {
        BusinessCalendarSet(name, Location.Unknown, days(name))
      }
    }
  }
}