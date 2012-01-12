package starling.daterange

import java.util.{GregorianCalendar, Calendar}
import java.lang.{Integer => jInt}
import starling.utils.cache.CacheFactory
import starling.utils.Log
import org.joda.time.format.{DateTimeFormatter, DateTimeFormat}
import starling.utils.StringToDouble
import org.joda.time.{LocalTime, DateTime, LocalDate}
import annotation.tailrec
import starling.calendar.{Clock, BusinessCalendar}

/**
 * calendar day.
 *
 * note that month and day numbers are 1-indexed, not zero.
 *
 * Don't construct this yourself, use the Day(...) case constructor-like syntax which uses a
 * pre-allocated array of days to reduce GC load.
 */
class Day private (@transient val year : Int, @transient val month : Int, @transient val dayNumber : Int) extends DateRange with Serializable {
  import DayOfWeek._
  import TimeOfDay._

  private def readResolve() : Object = Day.fromJulianDayNumber(julianDayNumber)

  def firstDay = this
  def lastDay = this
  def toSqlFormat : String = String.format("%04d-%02d-%02d", new jInt(year), new jInt(month), new jInt(dayNumber))
  def toOracleFormat : String = String.format("%02d-%s-%02d", new jInt(dayNumber), Month.shortMonthName(month), new jInt(year))
  def calendar : Calendar = {
    val calendar = new GregorianCalendar
    calendar.set(year, month - 1, dayNumber, 0, 0)
    calendar.set(Calendar.SECOND, 0)
    calendar.set(Calendar.MILLISECOND, 0)
    calendar
  }

  def add(num : Int, tenor : TenorType) = {
    var cal = calendar
    tenor match {
      case Month => cal.add(Calendar.MONTH, num)
      case Year => cal.add(Calendar.YEAR, num)
    }
    Day.fromJavaDate(cal.getTime)
  }

  def toSqlDate : java.sql.Date = {
    new java.sql.Date(calendar.getTimeInMillis)
  }

  implicit def toJodaLocalDate : LocalDate = {
    new LocalDate(this.year, this.month, this.dayNumber)
  }

  def toExcel = (this - Day(1899, 12, 30)).doubleValue

  private def toJodaDateTime : DateTime = {
    new DateTime(year, month, dayNumber, 0, 0, 0, 0)
  }

  def toLIM: String = month.toString + "/" + dayNumber.toString + "/" + year.toString
  def millis = toJodaDateTime.getMillis
  def toTimestamp = new Timestamp(millis)
  def toJavaDate = new java.util.Date(calendar.getTimeInMillis)
  def toLocalDate = new LocalDate(calendar.getTimeInMillis)

  def week = Week.containing(this)

  def toEndOfDayTimestamp = nextDay.toTimestamp - Seconds(1)

  val julianDayNumber = Day.julianDayNumber(this)
  override def hashCode = julianDayNumber

  override def equals(that: Any) = that match {
    case other : Day => {
      other.getClass == getClass && other.julianDayNumber == julianDayNumber
    }
    case _ => false
  }

  override def compare(that : DateRange) = that match {
    case thatDay : Day => {
      julianDayNumber - thatDay.julianDayNumber
    }
    case _ : DateRange => super.compare(that)
  }

  /// returns the earlier of two periods
  def min(that : Day) : Day = if (this < that) this else that

  /// returns the later of two periods
  def max(that : Day) : Day = if (this > that) this else that

  def daysSinceInYears(that : Day) = {
    /* Before using this function make sure it is what you need. For measuring time for
       volatility purposes you should almost certainly be using DayAndTime.timeSince. However
       for FX and interest rate calcs its use is appropriate as borowing costs are incurred
       during the instant between end of one day and start of the next. Whereas volatility
       happens between start and end of the same day
      */
    (julianDayNumber - that.julianDayNumber) / Day.daysInYear
  }
//  def timeSince(that : Day) = Day.timeBetween(that, this)
//
//  def timeSince(that: DayAndTime) = Day.timeBetween(that, this)

  /** Add a number of days to this day
   */
  def + (n : Int) : Day = {
    Day.fromJulianDayNumber(Day.julianDayNumber(this) + n)
  }

  /// returns a day +n+ days before this
  def - (n : Int) = this + (-n)

  /** returns the number of days between this and another day. Is negative if
   * 	other day1 is in the future
   */
  def - (d : Day) : Int = julianDayNumber - d.julianDayNumber

  def nextDay = this + 1
  def previousDay = this - 1

  def dayOfWeek = DayOfWeek(julianDayNumber % 7)

  def previousBusinessDay(bc : BusinessCalendar) : Day = bc.previousBusinessDay(this)
  def businessDays(bc: BusinessCalendar, days: Seq[Int]): Seq[Day] = days.map(addBusinessDays(bc, _))

  def thisOrPreviousBusinessDay(bc : BusinessCalendar) : Day = bc.thisOrPreviousBusinessDay(this)

  def nextBusinessDay(bc : BusinessCalendar) : Day = bc.nextBusinessDay(this)

  def thisOrNextBusinessDay(bc : BusinessCalendar) : Day = bc.thisOrNextBusinessDay(this)

  def isBusinessDay(bc : BusinessCalendar) : Boolean = bc.isBusinessDay(this)

  def isHoliday(bc : BusinessCalendar) : Boolean = bc.isHoliday(this)

  def nextWeekday : Day = {
    nextBusinessDay(BusinessCalendar.NONE)
  }

  def next(dayOfWeek: DayOfWeek): Day = this + (7 - (this.dayOfWeek - dayOfWeek))

  @tailrec
  final def addBusinessDays(bc : BusinessCalendar, n: Int): Day = {
    if (n > 0) {
      nextBusinessDay(bc).addBusinessDays(bc, n - 1)
    } else if (n < 0) {
      previousBusinessDay(bc).addBusinessDays(bc, n + 1)
    } else {
      this
    }
  }

  /**
   * Returns the number of business days between this day and the supplied day.
   */
  def businessDaysBetween(day:Day, bc:BusinessCalendar) = {
    if (!bc.isBusinessDay(this) || !bc.isBusinessDay(day))
      throw new IllegalArgumentException("Can only find business days between days that are business days: " + this + " : " + day + " : " + bc)
    var testDay = this
    var count = 0
    if (day == this) {
      0
    } else if (day < this) {
      while (day < testDay) {
        testDay = testDay.previousBusinessDay(bc)
        count -= 1
      }
      count
    } else {
      while (day > testDay) {
        testDay = testDay.nextBusinessDay(bc)
        count += 1
      }
      count
    }
  }

  def weekdaysBetween(day:Day) = {
    businessDaysBetween(day, BusinessCalendar.WeekdayBusinessCalendar)
  }

  def addWeekdays(n:Int):Day = {
    addBusinessDays(BusinessCalendar.WeekdayBusinessCalendar, n)
  }

  def previousWeekday : Day = {
    previousBusinessDay(BusinessCalendar.NONE)
  }

  /** Returns the first day of the given day of week, on or after this day
   */
  def dayOnOrAfter(dow : DayOfWeek) = {
    var daysToAdd = dow.number - dayOfWeek.number
    if (daysToAdd < 0)
      daysToAdd += 7
    this + daysToAdd
  }

  def containingMonth : Month = Month(year, month)
  def containingQuarter : Quarter = Quarter.containing(this)

  /** Adds a number of months using the same day number if valid, otherwise
   *  the last day of the month
   */
  def addMonths(n : Int) : Day = {
    val month = containingMonth + n
    Day(month.y, month.m, dayNumber min month.lastDay.dayNumber)
  }

  def endOfDay = atTimeOfDay(EndOfDay)
  def startOfDay = atTimeOfDay(StartOfDay)
  def atTimeOfDay(timeOfDay : TimeOfDay) = DayAndTime(this, timeOfDay)
  def atTimeOfDay(timeOfDay: ObservationTimeOfDay) = ObservationPoint(this, timeOfDay)
  def atTimeOfDay(time: LocalTime, location: Location): DateTime = time.toDateTimeToday(location.timeZoneOn(this))
  def asMonthObject = Month(year, month)

  /** Returns a simple date range from this day to the day supplied (inclusive). Will fail if day
   * 	supplied is in the past
   */
  def upto (lastDay : Day) = {
    require(lastDay >= this, lastDay + " >= " + this)
    SimpleDateRange(this, lastDay)
  }

  /** Returns a simple date range from this day to the day supplied (exclusive). Will fail if day
   * 	supplied is the same or in the past
   */
  def until (lastDay : Day) = {
    require(lastDay > this, lastDay + " is not after " + this)
    SimpleDateRange(this, lastDay - 1)
  }

  @transient lazy val isWeekendDay : Boolean = {
    dayOfWeek == saturday || dayOfWeek == sunday
  }

  @transient lazy val isWeekday : Boolean = ! isWeekendDay

  def isMonday = {
    julianDayNumber % 7 == 0;
  }

  def isTuesday = {
    julianDayNumber % 7 == 1;
  }

  def isWednesday = {
    julianDayNumber % 7 == 2;
  }

  def isThursday = {
    julianDayNumber % 7 == 3;
  }

  def isFriday = {
    julianDayNumber % 7 == 4;
  }

  def isSaturday = {
    julianDayNumber % 7 == 5;
  }

  def isSunday = {
    julianDayNumber % 7 == 6;
  }

  override def toString : String = {
    toString(Day.DEFAULT_PRINT_FORMAT)
  }

  def toString(pattern : String) = {
    toJodaLocalDate.toString(pattern)
  }

  def toString(dateTimeFormatter: DateTimeFormatter) = {
    toJodaLocalDate.toString(dateTimeFormatter)
  }

  def tenor = Some(Day)

  def containingYear = Year(year)

  def startOfFinancialYear = {
    if(this >= Day(year,10,1)) {
      Day(year, 10, 1)
    } else {
      Day(year-1, 10, 1)
    }
  }

  def toListOfMonths = throw new Exception("Day " + this + "Can't be broken into months")
}

case class DayParseException(s: String) extends Exception(s)

object Day extends TenorType {
  type T = Day

  implicit object ordering extends Ordering[Day] {
		def compare(lhs : Day, rhs : Day) : Int = DateRange.ordering.compare(lhs, rhs)
	}
  val DEFAULT_PRINT_FORMAT  = "ddMMMyyyy"

  /* hack to allow Day to be an instance of Ordered, even though DateRange is also and Ordered isn't
   * covariant. see http://stackoverflow.com/questions/1818777/extend-scala-class-that-extends-ordered
   * for more information.
   */
  implicit def orderedDR2orderedDay[Day <: DateRange with Ordered[DateRange]](d : Day) : Ordered[Day] = d.asInstanceOf[Ordered[Day]]

  def unapply(obj : Object) : Option[(Int, Int, Int)] = obj match {
    case d : Day => Some((d.year, d.month, d.dayNumber))
    case _ => None
  }

  def unapply(d: Double): Option[Day] = Some(Day.fromExcel(d))

  def unapply(str: String): Option[Day] = str match {
    case StringToDouble(d) => Some(Day.fromExcel(d))
    case _ => try {
      val d: Double = 0
      Some(parse(str))
    } catch {
      case _: DayParseException => None
    }
  }

  def fromSqlDate(date : java.sql.Date) = fromMillis(date.getTime)
  def fromJavaDate(date : java.util.Date) = fromMillis(date.getTime)
  def fromJodaDate(date : LocalDate) = Day(date.getYear, date.getMonthOfYear, date.getDayOfMonth)
  def fromLocalDate(date: LocalDate) = fromJavaDate(date.toDateMidnight.toDate)
  private val milliCache = CacheFactory.getCache("Day.millis")
  def fromMillis(millis: Long) : Day = {
//    milliCache.memoize( (millis), {
      val calendar = new GregorianCalendar
    	calendar.setTimeInMillis(millis)
  		Day(
  				calendar.get(Calendar.YEAR),
  				calendar.get(Calendar.MONTH)+ 1,
  				calendar.get(Calendar.DAY_OF_MONTH)
  		)
//    })
  }  
  /**
   * All days are pre-allocated in a 100 year block, which seems to cover all current usage.
   * This is basically a flyweight pattern which means that there will only ever be the same
   * number of allocated instances of Day and all usages are references to them. This helps
   * prevent memory churn when doing date-heavy processing (e.g: TreeSet/Map of DateRange)
   * and should therefore be nicer to the GC.
   */
  private val dayArrayStart = calculateJulianDayNumber(1980, 1, 1)
  private val dayArray = {
    val arrayEnd = calculateJulianDayNumber(2100, 1, 2)
    val array = new Array[Day](arrayEnd - dayArrayStart)
    for (jDay <- dayArrayStart until arrayEnd) {
      array(jDay - dayArrayStart) = constructFromJulianNumber(jDay)
    }
    array
  }

  /**
   * Looks like a constructor for Day, but actually looks it up in the pre-allocated array of
   * days.
   */
  def apply(year : Int, month : Int, day : Int) = {
    val dayNum = calculateJulianDayNumber(year, month, day)
    fromJulianDayNumber(dayNum)
  }

  def apply(year: String, month: String, day: String): Day = apply(year.toInt, month.toInt, day.toInt)

  def valueOf(string: String) = parse(string) // Used by Resteasy

  def fromExcel(double : Double): Day = {
    Day(1899, 12, 30) + double.toInt
  }

  def today = Clock.today

  def yesterday = today - 1
  def tomorrow = today + 1

  def julianDayNumber(day : Day) : Int = {
		calculateJulianDayNumber(day.year, day.month, day.dayNumber)
  }

  def fromJulianDayNumber(jdn : Int) : Day = {
    val dayIndex = jdn - dayArrayStart
    if (dayIndex < 0 || dayIndex >= dayArray.length) {
      val d = constructFromJulianNumber(jdn)
      if (!(d.year == 1899 && d.month == 12 && d.dayNumber == 30)) { //Excel day 0
        Log.warn("Day " + d + " is outside the pre-calculated range. Consider extending it?")
      }
      d
    } else dayArray(dayIndex)
  }

  /** think this algorithm comes from Numerical Recipes - in any
   * 	case it's old
   */
  private def calculateJulianDayNumber(year : Int, month : Int, d : Int) : Int = {
    var y = year
    var m = month
    if (m > 2) {
      m -= 3; // wash out the leap day
    } else {
      m += 9;
      y -= 1;
    }
    val c : Int = y / 100;
    val ya : Int = y - 100*c;
    ((146097*c)>>2)+((1461*ya)>>2)+(153*m + 2)/5 + d + 1721119;
  }

  /**
   * This is the only place where a Day instance is actually allocated.
   */
  private def constructFromJulianNumber(jdn : Int) : Day = {
    var j = jdn - 1721119;
    var year =(((j<<2)- 1)/ 146097);
    j =(j<<2)- 1 - 146097*year;
    var d =(j>>2);
    j =((d<<2)+ 3)/ 1461;
    d =(d<<2)+ 3 - 1461*j;
    d =(d + 4)>>2;
    var month =(5*d - 3)/153;
    d = 5*d - 3 - 153*month;
    var day =((d + 5)/5);
    year =(100*year+ j);
    if(month < 10){
        month += 3;
    } else {
        month -= 9;
        year += 1;
    }
    new Day(year, month, day);
  }

  val daysInYear:Double = 365.0
//  def timeBetween(day1 : Day, day2 : Day) = {
//    (julianDayNumber(day2) - julianDayNumber(day1)) / daysInYear
//  }

  import TimeOfDay._
//  def timeBetween(dayAndTime: DayAndTime, day2: Day) = dayAndTime match {
//    case DayAndTime(other, EndOfDay) => (julianDayNumber(day2) - julianDayNumber(dayAndTime.day)) / daysInYear
//    // TODO [09 Mar 2011] The difference between the same day on Start of day and End of day should be > 0
//    case DayAndTime(other, StartOfDay) => (julianDayNumber(day2) - julianDayNumber(dayAndTime.day)) / daysInYear
//  }


  def laterOf(a:Day, b:Day) = if (a > b) a else b

  @transient
  val patterns = List("ddMMMyy", "dd'-'MMM'-'yy", "yyyy'-'MM'-'dd", "dd MMM yy", "dd/MM/yy", "dd/MMM/yy")

  private def readResolve() : Object = Day

  /**
   * Expects a string in ddMMMyy format
   */
  def parse(dayStr: String): Day = dayStr match {
    case StringToDouble(d) if d > 2100 => {
      // if d is less than 2100 it's a year, otherwise it would be an excel date from about 1905
      Day.fromExcel(d)
    }
    case _ => {
      var parsedDay: Day = null
      patterns.find {
        p =>
          try {
            val pattern = DateTimeFormat.forPattern(p).withPivotYear(2050)
            // Dates sometimes have 'Z' at the end to indicate 'Zulu' time.
            val parseDayStr = if (dayStr.last == 'Z') {
              dayStr.substring(0, dayStr.length - 1)
            } else {
              dayStr
            }
            parsedDay = parseWithFormat(parseDayStr, pattern)
            true
          }
          catch {
            case _ => false
          }
      } match {
        case Some(_) => parsedDay
        case None => throw new DayParseException("Couldn't parse as day: " + dayStr)
      }
    }
  }

  def parseWithFormat(dayStr: String, format: String): Day = parseWithFormat(dayStr, DateTimeFormat.forPattern(format))

  def parseWithFormat(dayStr:String, format:DateTimeFormatter) = {
    val dateTime = format.parseDateTime(dayStr)
    Day(dateTime.getYear, dateTime.getMonthOfYear, dateTime.getDayOfMonth)
  }

  val standardFormat = DateTimeFormat.forPattern(DEFAULT_PRINT_FORMAT)

  def quickParse(dayStr : String) : Day = parseWithFormat(dayStr, standardFormat)

  def containing(d : Day) = d
  def difference(to : Day, from : Day) = to - from
  def add(n : Int, to : Day) = to + n

  implicit def dayLiteral(day: Int) = new {
    def Jan(year: Int) = Day(year, 1, day)
    def Feb(year: Int) = Day(year, 2, day)
    def Mar(year: Int) = Day(year, 3, day)
    def Apr(year: Int) = Day(year, 4, day)
    def May(year: Int) = Day(year, 5, day)
    def Jun(year: Int) = Day(year, 6, day)
    def Jul(year: Int) = Day(year, 7, day)
    def Aug(year: Int) = Day(year, 8, day)
    def Sep(year: Int) = Day(year, 9, day)
    def Oct(year: Int) = Day(year, 10, day)
    def Nov(year: Int) = Day(year, 11, day)
    def Dec(year: Int) = Day(year, 12, day)
  }

  override def toString = "Day"

  override def shortName = "D"
}