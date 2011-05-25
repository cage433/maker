package starling.daterange

import org.joda.time.{DateTime, DateTimeZone}
import java.io.Serializable
import starling.utils.{Named, StarlingEnum}

abstract class Location(val name: String) extends Serializable with Named {
  def timeZoneOn(day: Day): DateTimeZone
  def now = new DateTime(timeZoneOn(Day.today))
}

object Location extends StarlingEnum(classOf[Location]) {
  object London extends Location("London") {
    val gmt = DateTimeZone.forID("Etc/GMT")
    val bst = DateTimeZone.forID("Etc/GMT-1")
    def bstStart(year: Year) = Month(year.yearNumber, 3).lastSunday
    def bstEnd(year: Year) = Month(year.yearNumber, 10).lastSunday

    def timeZoneOn(day: Day) = if (day >= bstStart(day.containingYear) && day <= bstEnd(day.containingYear)) bst else gmt
  }

  object Shanghai extends Location("Shanghai") {
    def timeZoneOn(day: Day) = DateTimeZone.forID("Asia/Shanghai")
  }

  object Unknown extends Location("Uknown") {
    def timeZoneOn(day: Day) = throw new Exception("Unknown time zone")
  }
}