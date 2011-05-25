package starling.pricingschedule

import org.scalatest.testng.TestNGSuite
import org.testng.annotations._
import org.testng.Assert._
import starling.daterange.TestHolidays
import starling.daterange.Day._
import starling.calendar.{BusinessCalendarSet, BusinessCalendar, HolidayTablesFactory}

class Test6_6 extends TestHolidays {
  val UK = HolidayTablesFactory.holidayTables.UK

  @Test
  def test {
    val xml = <PricingSchedule xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
      <Algorithm xsi:type="ContinuousPricingScheduleAlgorithm">
        <NonPricingDayTreatment>
          <Friday>Ignore</Friday>
          <Saturday>Previous</Saturday>
          <Sunday>Next</Sunday>
          <Monday>Ignore</Monday>
          <Other>Ignore</Other>
        </NonPricingDayTreatment>
        <EventDateTreatment>
          <Friday>Ignore</Friday>
          <Saturday>Ignore</Saturday>
          <Sunday>Ignore</Sunday>
          <Monday>Ignore</Monday>
          <Other>Ignore</Other>
        </EventDateTreatment>
        <ExcludeFirstandLastQuote>false</ExcludeFirstandLastQuote>
        <EventDate>2006-09-13Z</EventDate>
        <WeekStarts>Monday</WeekStarts>
        <NumberOfPricingPeriods>10</NumberOfPricingPeriods>
        <PricingPeriodType>CalendarDay</PricingPeriodType>
        <CountingStart>Day</CountingStart>
        <CountEventAsDay>0</CountEventAsDay>
        <NumberOfCountingPeriods>3</NumberOfCountingPeriods>
        <CountingPeriodType>CalendarDay</CountingPeriodType>
        <CountDirection>After</CountDirection>
        <PricingDirection>After</PricingDirection>
        <IncludeEventDate>true</IncludeEventDate>
        <ApplyEventDateTreatmentToFirstPricingDay>false</ApplyEventDateTreatmentToFirstPricingDay>
      </Algorithm> <EventDateType>BL</EventDateType> <EndEventDateType>BL</EndEventDateType> <ParcelPricing>AllParcels</ParcelPricing>
    </PricingSchedule>
    val sched = PricingScheduleXMLParser.parse(xml, None)
    val period = sched.algorithm.calculateSchedule(UK)
    assertEquals(period, (16 Sep 2006 upto (25 Sep 2006)).toList)
    val pricing = sched.algorithm.applyPricingRule(UK, period)
    assertEquals(pricing, List((15 Sep 2006, 0.1), (18 Sep 2006, 0.2), (19 Sep 2006, 0.1), (20 Sep 2006, 0.1), (21 Sep 2006, 0.1), (22 Sep 2006, 0.2), (25 Sep 2006, 0.2)))
  }
}