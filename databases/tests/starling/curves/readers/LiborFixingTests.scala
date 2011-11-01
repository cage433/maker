package starling.curves.readers

import lim.LIBORFixing
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import starling.quantity.{Quantity, UOM}
import starling.utils.ImplicitConversions._
import UOM._
import starling.daterange._
import io.Source
import java.lang.String
import Day._
import org.scalatest.prop.Checkers
import org.scalacheck.{Gen, Arbitrary, Prop}
import Prop._
import org.testng.Assert

trait RichArbitrary {
  implicit def enrichArbitrary[T](arbitrary: Arbitrary[T]) = new {
    def filter(predicate: T => Boolean) = Arbitrary { arbitrary.arbitrary.filter(predicate) }
  }
}

class LiborFixingTests extends WordSpec with ShouldMatchers with Checkers with HolidaysSpec with RichArbitrary {
  "Overnight value date matches against file from BBA" in {
    expectedValueDays("Overnight_Fixing_Calendar_2011.txt").map { fixingEntry =>
      fixingEntry.valueDays.map { case ((tenor, fixingDay), expected) =>
        val fixing = fixingEntry.fixingFor(fixingDay)
        Assert.assertEquals(fixing.valueDay(tenor), expected, "Invalid value date, fixing: %s, tenor: %s" % (fixing, tenor))
      }
    }
  }

  private val dodgyFixings = Set(JPY → (21 Dec 2011), JPY → (29 Dec 2011))//, SEK → (29 Dec 2011), DKK → (29 Dec 2011))

  //println("Dodgy fixings: " + dodgyFixings)

  "Spot/Next value date matches against file from BBA" in {
    expectedValueDays("Spot_Next_Fixing_Calendar_2011.txt").filter(_.currency.isOneOf(LIBORFixing.currencies)).map { fixingEntry =>
      fixingEntry.valueDays.map { case ((tenor, fixingDay), expected) =>
        if (!dodgyFixings.contains(fixingEntry.currency, fixingDay)) {
          val fixing = fixingEntry.fixingFor(fixingDay)
          Assert.assertEquals(fixing.valueDay(tenor), expected, "Invalid value date, fixing: %s, tenor: %s" % (fixing, tenor))
        }
      }
    }
  }

  private implicit def arbitraryFixing: Arbitrary[LIBORFixing] = Arbitrary {
    for {
      currency <- Gen.oneOf(LIBORFixing.currencies.toSeq)
      fixingDay <- Gen.choose((03 Jan 2011).julianDayNumber, (30 Dec 2011).julianDayNumber)
    } yield LIBORFixing(Quantity(1, currency), Day.fromJulianDayNumber(fixingDay))
  }.filter(fixing => fixing.isBusinessDay)

  private implicit def arbitraryTenor: Arbitrary[Tenor] = Arbitrary { Gen.oneOf(Tenor.ON, Tenor(Week, 1)) }

  "maturity day of O/N == value day" in {
    check((fixing: LIBORFixing) => fixing.supportsOvernight ==> (fixing.maturityDay(Tenor.ON) == fixing.valueDay(Tenor.ON)))
  }

  "1M maturity day  == 1 month from value day" in {
    check((fixing: LIBORFixing) => {
      val maturityDay = fixing.maturityDay(Tenor.OneMonth)
      val valueDay = fixing.valueDay(Tenor.OneMonth)
      val oneMonthAfterValueDay = valueDay.addMonths(1).thisOrNextBusinessDay(fixing.combinedCalendar)

      (valueDay != valueDay.containingMonth.lastDay &&
       oneMonthAfterValueDay.containingMonth == valueDay.addMonths(1).containingMonth) ==> {
        (maturityDay == oneMonthAfterValueDay) :|
          ("maturityDay: %s, valueDay: %s: valueDay + 1M: %s" % (maturityDay, valueDay, oneMonthAfterValueDay))
      }
    })
  }

  "maturity days cannot roll-over into a new calendar month" in {
    val data = Map(
      (CAD, 26 Jan 2011, Tenor.OneMonth) → (28 Jan 2011, 28 Feb 2011),
      (CAD, 27 Jan 2011, Tenor.OneMonth) → (31 Jan 2011, 28 Feb 2011),
      (CAD, 28 Jan 2011, Tenor.OneMonth) → (1 Feb 2011, 1 Mar 2011),
      (CAD, 31 Jan 2011, Tenor.OneMonth) → (2 Feb 2011, 2 Mar 2011)
    )

    data.map { case ((currency, fixingDay, tenor), (expectedValueDay, expectedMaturityDay)) =>
      val actualValueDay: Day = fixingFor(currency, fixingDay).valueDay(tenor)
      val actualMaturityDay: Day = fixingFor(currency, fixingDay).maturityDay(tenor)
      if (actualValueDay != expectedValueDay || actualMaturityDay != expectedMaturityDay) {
        println("ohdear")
      }

      actualValueDay should be === expectedValueDay
      actualMaturityDay should be === expectedMaturityDay
    }
  }

  private def expectedValueDays(fileName: String) = {
    val lines = Source.fromURL(getClass.getResource(fileName)).getLines.toList

    var fixingEntries: List[FixingEntry] = Nil

    lines.foreach { line =>
      val split: List[String] = line.split("\t").toList
      if (!split.isEmpty) {
        fixingEntries = split(0) match {
          case "" => createFixingEntries(split)
          case "Fix Date" => getTenors(split).zipWith(fixingEntries)((tenors, fixingEntry) => fixingEntry.addTenors(tenors))
          case _ => getValueDays(split).zipWith(fixingEntries)((valueDays, fixingEntry) => fixingEntry.addValueDays(valueDays))
        }
      }
    }

    fixingEntries
  }

  private case class FixingEntry(currency: UOM, tenors: List[String] = Nil, valueDays: Map[(Tenor, Day), Day]= Map()) {
    def fixingFor(fixingDay: Day) = LIBORFixing(Quantity(1, currency), fixingDay)
    def addTenors(tenors: List[String]) = copy(tenors = this.tenors ::: tenors)
    def addValueDays(valueDays: List[(Day, Option[Day])]) = copy(valueDays = this.valueDays ++ getValueDays(valueDays))

    private def getValueDays(valueDays: List[(Day, Option[Day])]): List[((Tenor, Day), Day)] = {
      tenors.zip(valueDays).collect {
        case (tenor, (fixingDay, Some(valueDay))) => (parseTenor(tenor), fixingDay) → valueDay
      }
    }

    private def parseTenor(tenor: String) = tenor match {
      case "Overnight" => Tenor.ON
      case _ => Tenor.OneWeek
    }
  }

  private def createFixingEntries(data: List[String]): List[FixingEntry] = data.flatMapO(fromStringOption(_)).map(FixingEntry(_))
  private def fromStringOption(text: String) = if (text.isEmpty) None else UOM.fromStringOption(text)

  private def getTenors(data: List[String]) = {
    data.split(item => item.isEmpty, false).map(_.filter(_.isOneOf("Overnight", "1W - 12M", "All Maturities")))
  }

  private def getValueDays(data: List[String]): List[List[(Day, Option[Day])]] = {
    val parsedDays = data.split(item => item.isEmpty, false).map(_.map(Day.unapply(_)))

    (parsedDays: @unchecked) match {
      case (Some(fixingDay) :: vOne) :: valueDayGroups => (vOne :: valueDayGroups).map(_.map(x => (fixingDay, x)))
    }
  }

  private def fixingFor(currency: UOM, fixingDay: Day) = LIBORFixing(Quantity(1, currency), fixingDay)
}