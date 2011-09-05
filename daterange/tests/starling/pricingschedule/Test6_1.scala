package starling.pricingschedule

import org.testng.annotations.Test


class Test6_1 {

  @Test
  def test {
    val xml = <PricingSchedule xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
      <Algorithm xsi:type="ContinuousPricingScheduleAlgorithm">
        <NonPricingDayTreatment>
          <Friday>Ignore</Friday>
          <Saturday>Ignore</Saturday>
          <Sunday>Ignore</Sunday>
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
        <EventDate>2007-08-10Z</EventDate>
        <WeekStarts>Monday</WeekStarts>
        <NumberOfPricingPeriods>5</NumberOfPricingPeriods>
        <PricingPeriodType>WorkingDay</PricingPeriodType>
        <CountingStart>Day</CountingStart>
        <CountEventAsDay>0</CountEventAsDay>
        <NumberOfCountingPeriods>10</NumberOfCountingPeriods>
        <CountingPeriodType>CalendarDay</CountingPeriodType>
        <CountDirection>Before</CountDirection>
        <PricingDirection>Before</PricingDirection>
        <IncludeEventDate>true</IncludeEventDate>
        <ApplyEventDateTreatmentToFirstPricingDay>false</ApplyEventDateTreatmentToFirstPricingDay>
      </Algorithm>
      <Description>Test 6 1</Description>
      <EventDateType>BL</EventDateType>
      <EndEventDateType>BL</EndEventDateType>
      <ParcelPricing>AllParcels</ParcelPricing>
    </PricingSchedule>
    val sched = PricingScheduleXMLParser.parse(xml, None)
  }
}
