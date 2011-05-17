package starling.utils

/** Copied from the krt adapter code - This should be in an LShift library
 *
 */
import org.joda.time.format.ISODateTimeFormat

/**
 * Conversions allowing for working with dates.
 */
object DateConversions {
  /**
   * Allows a string to be treated as a Date Convertable String.
   */
  implicit def dateConvertableString(s:String) = new DateConvertableString(s);
}

class DateConvertableString(val s:String) {
  def date : org.joda.time.LocalDate =
    s match {
      case "" => null
      case _  => org.joda.time.format.DateTimeFormat.forPattern("yyyy-MM-dd").parseDateTime(s).toLocalDate
    };

  def datetime : org.joda.time.DateTime =
    s match {
      case "" => null
      case _  => ISODateTimeFormat.dateTime().parseDateTime(s)
    };

  def monthyear : org.joda.time.LocalDate =
    s match {
      case ""  => null
      case _   => org.joda.time.format.DateTimeFormat.forPattern("YYYY-MM").parseDateTime(s).toLocalDate
    };
}