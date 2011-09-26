package starling.daterange

import org.joda.time.{DateTime, DateTimeZone}
import java.io.Serializable
import starling.utils.StarlingEnum

/**
 * The Location class defines its enumeration's timeZone method for determining the DateTimeZone of a given Day.
 *
 * @documented
 */
abstract class Location(val name: String) extends Serializable {
  def timeZoneOn(day: Day): DateTimeZone
  def now = new DateTime(timeZoneOn(Day.today))
}

/**
 * Location is the Starling enumeration of 3 nested types each having a look-up by a defined, case sensitive name:
 * London, Shanghai and the Unknown location.  
 *
 * @see DateTimeZone
 * @documented
 */
object Location extends StarlingEnum(classOf[Location], (location:Location) => location.name) {
  /**
   * London is a Location defining its DTZ with respect to British Summer Time (BST).
   */
  object London extends Location("London") {
    val gmt = DateTimeZone.forID("Etc/GMT")
    val bst = DateTimeZone.forID("Etc/GMT-1")
    def bstStart(year: Year) = Month(year.yearNumber, 3).lastSunday
    def bstEnd(year: Year) = Month(year.yearNumber, 10).lastSunday

    def timeZoneOn(day: Day) = if (day >= bstStart(day.containingYear) && day <= bstEnd(day.containingYear)) bst else gmt
  }
  /**
   * Shanghai is a Location whose DTZ Id "Asia/Shanghai".
   */
  object Shanghai extends Location("Shanghai") {
    def timeZoneOn(day: Day) = DateTimeZone.forID("Asia/Shanghai")
  }
  /**
   * The Unknown location.
   */
  object Unknown extends Location("Unknown") {
    /** @throws An exception on each call. */
    def timeZoneOn(day: Day) = throw new Exception("Unknown time zone")
  }
}
