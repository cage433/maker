package starling.utils.conversions

import org.joda.time.LocalTime


trait RichLocalTime {
  implicit def enrichIntToLocalTime(hour: Int) = new {
    def H(minute: Int) = new LocalTime(hour, minute)
  }
}