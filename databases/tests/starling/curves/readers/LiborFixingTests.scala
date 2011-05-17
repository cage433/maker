package starling.curves.readers

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import starling.quantity.{Quantity, UOM}
import starling.utils.ImplicitConversions._
import UOM._
import starling.daterange._
import io.Source
import java.lang.String
import junit.framework.Assert
import Day._


class LiborFixingTests extends WordSpec with ShouldMatchers with HolidaysSpec {
  "Overnight value date matches against file from BBA" in {
    expectedValueDays("Overnight_Fixing_Calendar_2011.txt").map { fixingEntry =>
      fixingEntry.valueDays.map { case ((tenor, fixingDay), expectedValueDay) =>
        val fixing = LIBORFixing(Quantity(1, fixingEntry.currency), fixingDay)
        Assert.assertEquals("Invalid value date, fixing: %s, tenor: %s" % (fixing, tenor),
          expectedValueDay, fixing.valueDay(tenor))
      }
    }
  }

  val dodgyFixings = Set(JPY → (21 Dec 2011), JPY → (29 Dec 2011), SEK → (29 Dec 2011), DKK → (29 Dec 2011))

  println("Dodgy fixings: " + dodgyFixings)

  "Spot/Next value date matches against file from BBA" in {
    expectedValueDays("Spot_Next_Fixing_Calendar_2011.txt").map { fixingEntry =>
      fixingEntry.valueDays.map { case ((tenor, fixingDay), expectedValueDay) =>
        if (!dodgyFixings.contains(fixingEntry.currency, fixingDay)) {
          val fixing = LIBORFixing(Quantity(1, fixingEntry.currency), fixingDay)
          Assert.assertEquals("Invalid value date, fixing: %s, tenor: %s" % (fixing, tenor),
            expectedValueDay, fixing.valueDay(tenor))
        }
      }
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
    def addTenors(tenors: List[String]) = copy(tenors = this.tenors ::: tenors)
    def addValueDays(valueDays: List[(Day, Option[Day])]) = copy(valueDays = this.valueDays ++ getValueDays(valueDays))

    private def getValueDays(valueDays: List[(Day, Option[Day])]): List[((Tenor, Day), Day)] = {
      tenors.zip(valueDays).collect {
        case (tenor, (fixingDay, Some(valueDay))) => (parseTenor(tenor), fixingDay) → valueDay
      }
    }

    private val OneWeek = Tenor(Week, 1)

    private def parseTenor(tenor: String) = tenor match {
      case "Overnight" => Tenor.ON
      case _ => OneWeek
    }
  }

  private def createFixingEntries(data: List[String]): List[FixingEntry] = {
    data.map(fromStringOption(_)).somes.map(FixingEntry(_))
  }

  def fromStringOption(text: String) = if (text.isEmpty) None else UOM.fromStringOption(text)

  def getTenors(data: List[String]) = {
    data.split(item => item.isEmpty, false).map(_.filter(_.isOneOf("Overnight", "1W - 12M", "All Maturities")))
  }

  def getValueDays(data: List[String]): List[List[(Day, Option[Day])]] = {
    val parsedDays = data.split(item => item.isEmpty, false).map(_.map(Day.unapply(_)))

    parsedDays match {
      case (Some(fixingDay) :: vOne) :: valueDayGroups => (vOne :: valueDayGroups).map(_.map(x => (fixingDay, x)))
    }
  }
}