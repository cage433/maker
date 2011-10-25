package starling.daterange

import org.joda.time.DateTime
import java.io.Serializable
import starling.utils.StarlingEnum
import scalaz.Scalaz._

abstract class Location(val name: String) extends Serializable {
  def timeZoneOn(day: Day): TimeZone
  def now = new DateTime(timeZoneOn(Day.today))
}

object Location extends StarlingEnum(classOf[Location], (location:Location) => location.name) {
  object London extends Location("London") {
    def timeZoneOn(day: Day) = summerTime(day.containingYear).contains(day) ? TimeZone.BST | TimeZone.GMT

    private def summerTime(year: Year) = Month(year.yearNumber, 3).lastSunday upto Month(year.yearNumber, 10).lastSunday
  }

  object Shanghai extends Location("Shanghai") {
    def timeZoneOn(day: Day) = TimeZone.Shanghai
  }

  object NewYork extends Location("New York") {
    def timeZoneOn(day: Day) = TimeZone.USEastern
  }

  object Unknown extends Location("Unknown") {
    def timeZoneOn(day: Day) = throw new Exception("Unknown time zone")
  }
}