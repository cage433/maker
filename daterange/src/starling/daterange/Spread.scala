package starling.daterange

/**
 * A spread of arbitrary date ranges.
 *
 * Although a spread is a range of dates it is not a DateRange as it is not contiguous. i.e. A Jan/Mar
 * spread does not have Feb as part of the Spread. If you want something that is a range of dates
 * a Strip might be more suitable.
 *
 */
case class Spread[T <: DateRange](
        first: T,
        last: T
        ) {
  private lazy val tenor = first.tenor match {
    case Some(t) => t
    case None => throw new Exception("This Spread does not have a defined tenor: " + this)
  }

  def hasTenor = {
    first.tenor.isDefined && first.tenor == last.tenor
  }

  /**
   * Next spread. Useful when people talk about strips of spreads.
   *
   * This can only work if this spread has a defined tenor and the tenor is the same for first and last.
   *
   * e.g. Jan11/Feb11 next gives Feb11/Mar11
   */
  def next: Spread[T] = {
    assert(hasTenor, "Iterable Spreads must be on DateRanges with the same defined Tenors: " + this)
    val f = tenor.add(1, first.asInstanceOf[tenor.T])
    val l = tenor.add(1, last.asInstanceOf[tenor.T])
    Spread(f, l).asInstanceOf[Spread[T]]
  }

  /**
   * Can iterate over a Spread as if it was a Strip.
   * A Spread on Oct10/Jan11 becomes a list of Spreads on Oct10/Nov10, Nov10/Dec10, Dec10,Jan11
   *
   * This can only work if this spread has a defined tenor and the tenor is the same for first and last.
   *
   * This class is not iterable because the types wouldn't make sense. We're returning a copy of ourselves
   * as we iterate not a value of type T
   */
  def iterator = new Iterator[Spread[_ <: DateRange]] {
    assert(hasTenor, "Iterable Spreads must be on DateRanges with the same defined Tenors: " + this)

    var nextVal: Spread[_ <: DateRange] = Spread(first, tenor.add(1, first.asInstanceOf[tenor.T]))

    def next = {
      val current = nextVal
      nextVal = current.next
      current
    }

    def hasNext: Boolean = nextVal.last <= last
  }

  override def toString = first + "/" + last
}

object Spread {
  // very verbose way of saying that spreads sort like their first element, then their last element.
  implicit def Spread[T <: DateRange]: Ordering[Spread[T]] =
    new Ordering[Spread[T]] {
      def compare(lhs: Spread[T], rhs: Spread[T]) = {
        val firstOrdered = lhs.first compare rhs.first
        if (firstOrdered == 0) {
          lhs.last compare rhs.last
        } else {
          firstOrdered
        }
      }
    }

  val SpreadRegex1 = """(?i)(.*)[ ]?/[ ]?(.*)""".r
  val SpreadRegex2 = """(?i)([a-z]{1}\d)([a-z]{1}\d)""".r

  def parse(text: String): Option[Spread[_ <: DateRange]] = {
    text match {
      case SpreadRegex1(first, last) => (first, last) match {  // Spread between two tenors separated by /
        case (TenorType(t1), TenorType(t2)) => {
          if(first.length == 2 && t1.tenor == Some(Year) && t2.tenor == Some(Year)) {
            // because 04/11 will look like a spread between two years but is actually a month
            None
          } else {
            Some(new Spread(t1, t2))
          }
        }
        case (DateRange(t1), DateRange(t2)) => {
          Some(new Spread(t1, t2))
        }
        case _ => None
      }
      case SpreadRegex2(first, last) => (first, last) match { // Spread without a / i.e. mar11apr11
        case (TenorType(t1), TenorType(t2)) => Some(new Spread(t1,t2))
        case _ => None
      }
      case _ => None
    }
  }
}

object SpreadParse {
  def unapply(s: String) = Spread.parse(s)
}