package starling.utils.conversions

import org.joda.time.{Duration, ReadableInstant, DateTime}

trait RichDateTime {
  implicit def enrichDateTime(dateTime: DateTime) = new {
    def <(other: ReadableInstant) = dateTime.isBefore(other)
    def <(other: Long) = dateTime.isBefore(other)
    def >(other: ReadableInstant) = dateTime.isAfter(other)
    def >(other: Long) = dateTime.isAfter(other)
    def +(other: Long) = dateTime.plus(other)
    def -(other: ReadableInstant) = new Duration(other, dateTime).getStandardSeconds
  }
}