package starling.daterange
import starling.utils.OrderedComparable
import collection.IterableView
import starling.calendar.BusinessCalendar
import starling.utils.cache.CacheFactory

/** the supertype of all date ranges
 *
 * IterableView is implemented purely to avoid a problem with using days in the repl.
 * see https://lampsvn.epfl.ch/trac/scala/ticket/3710#comment:14
 */
trait DateRange extends Iterable[Day] with OrderedComparable[DateRange] with IterableView[Day, Seq[Day]]{
  
  def firstDay : Day
  def lastDay : Day
  def firstMonth : Month = firstDay.containingMonth
  def lastMonth : Month = lastDay.containingMonth
  def tenor : Option[TenorType]

  def toShortString = toString

  protected def underlying = throw new Exception("Should never be called - hack to fix https://lampsvn.epfl.ch/trac/scala/ticket/3710#comment:14")

  def iterator() : Iterator[Day] = new Iterator[Day]{
    var currentDay = firstDay
    def hasNext = currentDay <= lastDay
    def next = {
      val theNextDay = currentDay
      currentDay = currentDay + 1
      theNextDay
    }
  }
  /** the intersection of this with some other date range
   */
  def intersection(rhs : DateRange) : Option[DateRange] = {
    DateRange.intersection(this, rhs)
  }
  /** the days in this date range
   */
  def days : Seq[Day] = iterator.toSeq

  def contains(day : Day) = firstDay <= day && day <= lastDay

  // since dateranges are contiguous
  def contains(range : DateRange) : Boolean = contains(range.firstDay) && contains(range.lastDay)
  def isContainedIn(range : DateRange) = range.contains(this)
  override def hashCode : Int = firstDay.hashCode ^ lastDay.hashCode

  override def equals(that: Any) = that match {
    case other : DateRange => {
      other.getClass == getClass && other.firstDay == firstDay && other.lastDay == lastDay
    }
    case _ => {
      false
    }
  }

  def compare(rhs : DateRange) : Int = DateRange.ordering.compare(this, rhs)

  /// returns the earlier of two periods
  def min(that : DateRange) : DateRange = if (this < that) this else that

  /// returns the later of two periods
  def max(that : DateRange) : DateRange = if (this > that) this else that

  //def timeLength = lastDay.timeSince(firstDay)
  override def size = lastDay - firstDay + 1
  def toListOfMonths : List[Month]

  def canSplitIntoMonths: Boolean = try {
    toListOfMonths
    true
  }
  catch {
    case e => false
  }

  /**
   * Shouldn't call this. Try creating a DateRange(start, end) instead and it will be called
   * automatically
   */
  def tryToNormalise: DateRange = {
    normalise match {
      case Some(tenor) => tenor
      case None => this
    }
  }

  def normalise: Option[DateRange] = {
    TenorType.ALL.flatMap {
      tenor =>
        val tenor1 = tenor.containing(firstDay)
        val tenor2 = tenor.containing(lastDay)
        if(tenor1 == tenor2 && tenor1.firstDay == firstDay && tenor2.lastDay == lastDay) {
          Some(tenor1)
        } else {
          None
        }
    } match {
      case Nil => None
      case tenor::Nil => Some(tenor)
      case mult => throw new Exception("Too many tenor matches for " + this + ": " + mult)
    }
  }

  def remainder(dayFromInclusive : Day) : Option[DateRange] = {
    if (dayFromInclusive > lastDay)
      None
    else if (dayFromInclusive <= firstDay)
      Some(this)
    else
      Some(DateRange(dayFromInclusive, lastDay))
  }
}

object DateRange {
  /**
   * Creates a date range and tries to return the normalised form.
   * E.g. DateRange(1Jul2010, 31Jul2010) would return Month(2010, 7)
   * Anything that doesn't have a normalised form will just be returned as a SimpleDateRange
   */
  def apply(firstDay : Day, lastDay : Day): DateRange = {
    SimpleDateRange(firstDay, lastDay).tryToNormalise
  }

  def apply(days: Iterable[Day]): DateRange = {
    assert(days.nonEmpty)
    SimpleDateRange(days.head, days.last).tryToNormalise
  }

  def unapply(s: String): Option[DateRange] = try {
    Some(parse(s))
  }
  catch {
    case e => None
  }

  val intersectionCache = CacheFactory.getCache("DateRange#interection")

  def intersection(lhs:DateRange, rhs : DateRange) : Option[DateRange] = {
    //400ms for reading EAI with London Derivatives Options + London Derivatives 17Dec2010
    intersectionCache.memoize( Set(rhs,lhs), {
      if (rhs.firstDay > lhs.lastDay || rhs.lastDay < lhs.firstDay)
        None
      else {
        val startDay : Day = lhs.firstDay max rhs.firstDay
        val endDay : Day = lhs.lastDay min rhs.lastDay
        Some(DateRange(startDay, endDay))
      }
    })
  }

  def parse(text : String):DateRange = {
    val split = text.split("-")
    if (split.size == 2) {
      new SimpleDateRange(Day.parse(split(0).trim), Day.parse(split(1).trim))
    } else {
      TenorType.parseTenor(text)
    }
  }
  
  implicit object ordering extends Ordering[DateRange]{
    def compare(lhs : DateRange, rhs : DateRange) : Int = {
      val firstCompare = lhs.firstDay compare rhs.firstDay
      if (firstCompare == 0)
        // reversing the comparison on the last day means longer date ranges sort
        // before shorter ones, which is as good an ordering as any.
        -(lhs.lastDay compare rhs.lastDay)
      else
        firstCompare
    }
  }

  
}
