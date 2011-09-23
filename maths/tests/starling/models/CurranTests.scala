package starling.models

import org.scalatest.testng.TestNGSuite
import starling.quantity.utils.QuantityTestUtils._
import starling.quantity.Quantity
import org.testng.Assert._
import starling.daterange._
import starling.calendar.HolidayTablesFactory
import org.testng.annotations._

class CurranTests extends TestHolidays {
  val daysInYear = Day.daysInYear

  lazy val calendarNYMEX = HolidayTablesFactory.holidayTables.NYMEX

  lazy val Dec10 = Month(2010, 12)
  lazy val pricingDaysDec10 = Dec10.days.filter(calendarNYMEX.isBusinessDay)

  lazy val Dec10Ti = pricingDaysDec10.map((day: Day) => day.endOfDay.timeSince(Day(2010, 1, 5).endOfDay))
  lazy val Dec10T = Dec10Ti.last

  lazy val Jan10observationDate = Day(2010, 1, 18)
  lazy val Jan10 = Month(2010, 1)
  lazy val pricingDaysJan10 = Jan10.days.filter(calendarNYMEX.isBusinessDay).filter(_ >= Jan10observationDate)
  lazy val Jan10Ti = pricingDaysJan10.map((day: Day) => day.endOfDay.timeSince(Jan10observationDate.endOfDay))
  lazy val Jan10T = Jan10Ti.last

  @Test
  def AtTheMoneyBeforePricingCurranCallTest = {
    implicit val daysInYear: Double = 365

    val c = new Curran(Call, 100, 100, 0.2, Dec10Ti.toArray, Dec10T, 0.0, 0)
    assertEquals(c.discountedValue(0.05), 7.310622, 1e-6)
  }
  
  @Test
	def AtTheMoneyBeforePricingCurranPutTest = {
		val c = new Curran(Put, 100, 100, 0.2, Dec10Ti.toArray, Dec10T, 0.0, 0)
		assertEquals(c.discountedValue(0.05), 7.310622, 1e-6)
	}

	@Test
	def AtTheMoneyBeforePricingCurranStraddleTest = {
		val c = new Curran(Straddle, 100, 100, 0.2, Dec10Ti.toArray, Dec10T, 0.0, 0)
		assertEquals(c.discountedValue(0.05), 14.621245, 1e-6)
	}

	@Test
	def InTheMoneyBeforePricingCurranCallTest = {
		val c = new Curran(Call, 150, 100, 0.2, Dec10Ti.toArray, Dec10T, 0.0, 0)
		assertEquals(c.discountedValue(0.05), 47.744130, 1e-6)
	}

	@Test
	def OutOfTheMoneyBeforePricingCurranPutTest = {
		val c = new Curran(Put, 150, 100, 0.2, Dec10Ti.toArray, Dec10T, 0.0, 0)
		assertEquals(c.discountedValue(0.05), 0.143551, 1e-6)
	}

	@Test
	def InTheMoneyHighBeforePricingCurranStraddleTest = {
		val c = new Curran(Straddle, 150, 100, 0.2, Dec10Ti.toArray, Dec10T, 0.0, 0)
		assertEquals(c.discountedValue(0.05), 47.887681, 1e-6)
	}

	@Test
	def OutOfTheMoneyBeforePricingCurranCallTest = {
		val c = new Curran(Call, 50, 100, 0.2, Dec10Ti.toArray, Dec10T, 0.0, 0)
		assertEquals(c.discountedValue(0.05), 0.000515, 1e-6)
	}

	@Test
	def OutOfMoneyBeforePricingCurranPutTest = {
		val c = new Curran(Put, 50, 100, 0.2, Dec10Ti.toArray, Dec10T, 0.0, 0)
		assertEquals(c.discountedValue(0.05), 47.601094, 1e-6)
	}

	@Test
	def InTheMoneyLowBeforePricingCurranStraddleTest = {
		val c = new Curran(Straddle, 50, 100, 0.2, Dec10Ti.toArray, Dec10T, 0.0, 0)
		assertEquals(c.discountedValue(0.05), 47.601609, 1e-6)
	}

	// During pricing tests

	@Test
	def AtTheMoneyDuringPricingCurranCallTest = {
		val c = new Curran(Call, 100, 100, 0.2, Jan10Ti.toArray, Jan10T, 105.0, 10)
		assertEquals(c.discountedValue(0.05), 2.628647, 1e-6)
	}

	@Test
	def AtTheMoneyDuringPricingCurranPutTest = {
		val c = new Curran(Put, 100, 100, 0.2, Jan10Ti.toArray, Jan10T, 105.0, 10)
		assertEquals(c.discountedValue(0.05), 0.001030, 1e-6)
	}

	@Test
	def AtTheMoneyDuringPricingCurranStraddleTest = {
		val c = new Curran(Straddle, 100, 100, 0.2, Jan10Ti.toArray, Jan10T, 105.0, 10)
		assertEquals(c.discountedValue(0.05), 2.629678, 1e-6)
	}

	@Test
	def InTheMoneyDuringPricingCurranCallTest = {
		val c = new Curran(Call, 150, 100, 0.2, Jan10Ti.toArray, Jan10T, 105.0, 10)
		assertEquals(c.discountedValue(0.05), 26.276165, 1e-6)
	}

	@Test
	def OutOfTheMoneyDuringPricingCurranPutTest = {
		val c = new Curran(Put, 150, 100, 0.2, Jan10Ti.toArray, Jan10T, 105.0, 10)
		assertEquals(c.discountedValue(0.05), 0.0, 1e-6)
	}

	@Test
	def InTheMoneyHighDuringPricingCurranStraddleTest = {
		val c = new Curran(Straddle, 150, 100, 0.2, Jan10Ti.toArray, Jan10T, 105.0, 10)
		assertEquals(c.discountedValue(0.05), 26.276165, 1e-6)
	}

	@Test
	def OutOfTheMoneyDuringPricingCurranCallTest = {
		val c = new Curran(Call, 50, 100, 0.2, Jan10Ti.toArray, Jan10T, 105.0, 10)
		assertEquals(c.discountedValue(0.05), 0.0, 1e-6)
	}

	@Test
	def OutOfMoneyDuringPricingCurranPutTest = {
		val c = new Curran(Put, 50, 100, 0.2, Jan10Ti.toArray, Jan10T, 105.0, 10)
		assertEquals(c.discountedValue(0.05), 21.020932, 1e-6)
	}

	@Test
	def InTheMoneyLowDuringPricingCurranStraddleTest = {
		val c = new Curran(Straddle, 50, 100, 0.2, Jan10Ti.toArray, Jan10T, 105.0, 10)
		assertEquals(c.discountedValue(0.05), 21.020932, 1e-6)
	}
}
