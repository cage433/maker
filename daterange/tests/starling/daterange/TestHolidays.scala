package starling.daterange

import org.testng.annotations.{AfterTest, BeforeSuite, AfterSuite}
import starling.calendar.{BusinessCalendarSet, HolidayTablesFactory, HolidayTables}
import org.scalatest.testng.TestNGSuite

class TestHolidays extends TestNGSuite with HolidaysSpec

trait HolidaysSpec {
  var old: Option[HolidayTables] = HolidayTablesFactory.holidaysTablesOption

  val nymex = Set(
			Day(2009, 1, 1),
			Day(2009, 1, 19),
			Day(2009, 2, 16),
			Day(2009, 4, 10),
			Day(2009, 5, 25),
			Day(2009, 7, 3),
			Day(2009, 9, 7),
			Day(2009, 11, 26),
			Day(2009, 12, 25),
			Day(2010, 1, 1),
			Day(2010, 1, 18),
			Day(2010, 2, 15),
			Day(2010, 4, 2),
			Day(2010, 5, 31),
			Day(2010, 7, 5),
			Day(2010, 9, 6),
			Day(2010, 11, 25),
			Day(2010, 12, 24),
			Day(2010, 12, 31),
			Day(2011, 1, 17),
			Day(2011, 2, 21),
			Day(2011, 4, 22),
			Day(2011, 5, 30),
			Day(2011, 7, 4),
			Day(2011, 9, 5),
			Day(2011, 11, 24),
			Day(2011, 12, 26),
			Day(2012, 1, 2),
			Day(2012, 1, 16),
			Day(2012, 2, 20),
			Day(2012, 4, 6),
			Day(2012, 5, 28),
			Day(2012, 7, 4),
			Day(2012, 9, 3),
			Day(2012, 11, 22),
			Day(2012, 12, 25),
			Day(2013, 1, 1),
			Day(2013, 1, 21),
			Day(2013, 2, 18),
			Day(2013, 3, 29),
			Day(2013, 5, 27),
			Day(2013, 7, 4),
			Day(2013, 9, 2),
			Day(2013, 11, 28),
			Day(2013, 12, 25),
			Day(2014, 1, 1),
			Day(2014, 1, 20),
			Day(2014, 2, 17),
			Day(2014, 4, 18),
			Day(2014, 5, 26),
			Day(2014, 7, 4),
			Day(2014, 9, 1),
			Day(2014, 11, 27),
			Day(2014, 12, 25))

  val ARE = Set(Day(2011, 1, 3), Day(2011, 1, 3), Day(2011, 4, 22), Day(2011, 4, 25), Day(2011, 4, 29))

  HolidayTablesFactory.registerNewHolidayTablesImplForTesting(Some(new HolidayTables {
    def financialHolidaysOption(name: String) = Some(name match {
      case "NYM" => BusinessCalendarSet(name, Location.Unknown, nymex)
      case "ARE" => BusinessCalendarSet(name, Location.Unknown, ARE)
      case "IPE" => BusinessCalendarSet(name, Location.Unknown, Set(Day(2011, 4, 22)))
      case "SFS" => BusinessCalendarSet(name, Location.Unknown, Set(Day(2011, 4, 4), Day(2011, 4, 5)))
      case _ => BusinessCalendarSet(name, Location.Unknown, Set())
    })

    def regionHolidays(name: String) = BusinessCalendarSet(name, Location.Unknown, Set())
  }))

  //@AfterTest
  //def after {
    //HolidayTablesFactory.registerNewHolidayTablesImplForTesting(old)
    //}

}
