package starling.daterange

import starling.utils.cache.CacheFactory
import starling.utils.ImplicitConversions._
import starling.utils.Pattern.Extractor

/**
 * Tenor represents a class of repeating date ranges, for example days, weeks, months.
 *
 * These periods should completely cover all available days with no overlaps and each individual
 * date range of a tenor should be contiguous. Examples of things which are tenors: days, weeks,
 * months, years, decades. Examples of things which are not tenors: every monday, the third
 * wednesday of the month.
 */
trait TenorType {
  type T <: DateRange

  // return an instance of this tenor containing the given day
  def containing(d : Day) : T

  // return an instance of this tenor containing the whole given date range, if possible
  def containing(range : DateRange) : Option[T] = {
    val firstContaining = containing(range.firstDay)
    if (firstContaining.contains(range.lastDay))
      Some(firstContaining)
    else
      None
  }

  def difference(to : T, from : T) : Int
  def add(n : Int, from : T) : T

  def parse(s : String) : T

  // returns a list of all periods of this tenor type which intersect the given range
  def intersectingPeriods(range : DateRange) : List[T] = {
    val first = containing(range.firstDay)
    val last = containing(range.lastDay)
    (0 to difference(last, first)).map(i => add(i, first)).toList
  }

  def shortNames: List[String] = List(shortName)
  def shortName: String = shortNames.head
}

object TenorType {
  val ALL: List[TenorType] = List(Month, Day, Year, Week, HalfMonth, BOM, Quarter, HalfYear)
  val typesByShortName: Map[String, TenorType] = ALL.toMapWithManyKeys(_.shortNames)
  val FromShortName: Extractor[String, TenorType] = typesByShortName.toExtractor

  // same as above, but in (roughly) ascending order of length
  val ALL_IN_ORDER: List[String] = List(Day, Week, HalfMonth, BOM, Month, Quarter, HalfYear, Year).map(_.shortName)

  private var periodsCache = CacheFactory.getCache("Tenor.periodsCache", unique = true)

  def unapply(text: String) = periodsCache.memoize((text.trim),
    (tuple: (String)) => {
      ALL.flatMap {
        t => try {
          Some(t.parse(text.trim))
        } catch {
          case e => None
        }
      } match {
        case Nil => None
        case parsed :: Nil => Some(parsed)
        case matches => throw new IllegalStateException("Too many matches for " + text + ", " + matches)
      }
    })

  def parseTenor(text: String) = text match {
    case TenorType(t) => t
    case _ => throw new IllegalStateException("Can't parse text " + text)
  }

  def parseTenorName(tenorName: String) = ALL.filter(_.toString.toLowerCase == tenorName.toLowerCase) match {
    case m :: Nil => m
    case Nil => throw new Exception("Tenor name " + tenorName + " doesn't match any tenors.")
    case o => throw new Exception("Tenor name " + tenorName + " doesn't matches too many tenors: " + o)
  }

  def parseInterval(text: String): (Int, TenorType) = {
    val num = text.substring(0, text.size - 1).toInt
    val tenor = text.last match {
      case 'M' => Month
      case 'Y' => Year
      case 'D' => Day
    }
    (num, tenor)
  }
}