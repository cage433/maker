package starling.daterange

import org.testng.Assert._
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.{DataProvider, Test}
import starling.utils.ScalaTestUtils._
import starling.daterange.Day._

class PeriodTests extends TestNGSuite {

  @DataProvider(name = "ExcelRowTestsSimple")
  def data1 = {
    constructArgs(
      ("cal-11", Year(2011)),
      ("40544.0", Day(2011, 1, 1)),
      ("Jan-2010", Month(2010, 1)),
      ("F0", Month(2010, 1)),
      ("JAN-2010 BACK", new HalfMonth(2010, 1, FrontOrBack.Back)),
      ("10 back", new HalfYear(2010, FrontOrBack.Back)),
      ("h2-0", new HalfYear(2010, FrontOrBack.Back)),
      ("1h-10", new HalfYear(2010, FrontOrBack.Front)),
      ("q4-89", Quarter(1989, 4)),
      ("Q111", Quarter(2011, 1)),
      ("Y09", Year(2009)),
      ("Cal99", Year(1999)),
      ("09W12", Week(2009, 12)),
      ("1Mar10- 3Apr12", DateRange(1 Mar 2010, 3 Apr 2012)),
      ("01/2010", Month(2010, 1)),
      ("01/10", Month(2010, 1))
    )
  }

  @Test(dataProvider = "ExcelRowTestsSimple")
  def testParse1(s: String, d: DateRange) {
    val parsed = Period.unapply(s)
    assertEquals(parsed, Some(DateRangePeriod(d)))
  }

  @DataProvider(name = "ExcelRowTestsStrips")
  def data2 = {
    constructArgs(
      ("q410-q311", StripPeriod(Quarter(2010, 4), Quarter(2011, 3))),
      ("jan11-x2", StripPeriod(Month(2011, 1), Month(2012, 11))),
      ("2011-2012", StripPeriod(Year(2011), Year(2012))),
      ("u1 -> v1", StripPeriod(Month(2011, 9), Month(2011, 10))),
      ("u1tov1", StripPeriod(Month(2011, 9), Month(2011, 10)))
    )
  }

  @Test(dataProvider = "ExcelRowTestsStrips")
  def testParse2(s: String, p: Period) {
    val parsed = Period.unapply(s)
    assertEquals(parsed, Some(p))
  }

  @DataProvider(name = "ExcelRowTestsSpreads")
  def data3 = {
    constructArgs(
      ("q410/q311", SpreadPeriod(Quarter(2010, 4), Quarter(2011, 3))),
      ("jan11/x2", SpreadPeriod(Month(2011, 1), Month(2012, 11))),
      ("2011/2012", SpreadPeriod(Year(2011), Year(2012))),
      ("u1 / v1", SpreadPeriod(Month(2011, 9), Month(2011, 10))),
      ("u1/v1", SpreadPeriod(Month(2011, 9), Month(2011, 10)))
    )
  }

  @Test(dataProvider = "ExcelRowTestsSpreads")
  def testParse3(s: String, p: Period) {
    val parsed = Period.unapply(s)
    assertEquals(parsed, Some(p))
  }

}