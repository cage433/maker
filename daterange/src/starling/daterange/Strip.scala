package starling.daterange

/**
 * A strip is a list of Periods with the same type.
 */
case class Strip[T <: Period](
        override val first : T,
        override val last : T
        ) extends Iterable[T] {
  assert(first.getClass != classOf[StripPeriod], "Can't have strips of strips")
  assert(first.getClass == last.getClass, "Strips must be between two Periods of the same type")
  assert(first <= last, "First range of Strip must be before or equal last")

  override def toString = first + "-" + last

  def iterator = new Iterator[T] {
    var nextVal : T = first

    def next : T = {
      val current : T = nextVal
      nextVal = (current) match {
        case (f:DateRangePeriod) => f.next.asInstanceOf[T]
        case (f:SpreadPeriod) => f.next.asInstanceOf[T]
      }

      current
    }

    def hasNext : Boolean = nextVal <= last
  }

  def asDateRange =  (first, last) match {
        case (f:DateRangePeriod, l:DateRangePeriod) => new SimpleDateRange(f.period.firstDay, l.period.lastDay)
        case (f:SpreadPeriod, l:SpreadPeriod) => new SimpleDateRange(f.front.firstDay, l.back.lastDay)
      }
}

object Strip {
  def apply(first: DateRange, last: DateRange) = new Strip(DateRangePeriod(first), DateRangePeriod(last))

  def apply(first: Spread[_ <: DateRange], last: Spread[_ <: DateRange]) =
    new Strip(SpreadPeriod(first.first, first.last), SpreadPeriod(last.first, last.last))

  implicit def Strip[T <: Period] : Ordering[Strip[T]] =
    new Ordering[Strip[T]] {
      def compare(lhs: Strip[T], rhs: Strip[T]) = lhs.asDateRange.compare(rhs.asDateRange)
    }

  val StripRegex = """(?i)([a-z0-9 /]+)[ ]?(\-\>|\-|to)[ ]?(.*)""".r
  def parse(text: String): Option[Strip[_ <: Period]] = {
    try {
      text match {
        case StripRegex(from, _, to) => (from, to) match {
          case (TenorType(first), TenorType(last)) => (first, last) match {
            case (_: Day, _: Day) => None // "day1 - day2" is a date range not a strip
            case (_: Year, _: Year) if from.length == 2 => None // "04 - 11" is a date not a strip
            case (a: DateRange, b: DateRange) if a.tenor != b.tenor => None // not a strip between tenors
            case _ => Some(new Strip(first, last))
          }
          case (SpreadParse(first), SpreadParse(last)) => Some(new Strip(first, last))
          case _ => None
        }
        case _ => None
      }
    }
    catch {
      case e:AssertionError => None // because things like q1-11 look like strips but aren't valid
    }
  }
}

object StripParse {
  def unapply(s: String) = Strip.parse(s)
}