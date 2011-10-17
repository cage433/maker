package starling.utils.conversions

import org.joda.time.LocalTime

class RichLTime(hour : Int) {
  def H(minute: Int) = new LocalTime(hour, minute)
}

trait RichLocalTime {
  implicit def enrichIntToLocalTime(hour: Int) : RichLTime = new RichLTime(hour)
}
