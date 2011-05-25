package starling.pricingschedule

import starling.daterange.{DayOfWeek, Day}
import xml.{Node, Elem, XML}

object PricingScheduleXMLParser {

  def parse(xmlStr: String, givenEventDate: Option[Day]): PricingSchedule = {
    parse(XML.loadString(xmlStr), givenEventDate)
  }

  def parse(_xml: Elem, givenEventDate: Option[Day]): PricingSchedule = {

    val xml = scala.xml.Utility.trim(_xml)
    val description = xml \ "Description" text
    val eventDateType = xml \ "EventDateType" text
    val endEventDateType = xml \ "EndEventDateType" text
    val parcelPricing = ParcelPricingOption.parse((xml \ "ParcelPricing").head.child.text).get

    val algorithm = xml \ "Algorithm" \ "@{http://www.w3.org/2001/XMLSchema-instance}type" text match {
      case "ContinuousPricingScheduleAlgorithm" => {
        val nonPricingDayTreatment = getNonPricingDayTreatment(xml)
        val eventDateTreatment = getEventDateTreatment(xml)

        val excludeFirstandLastQuote = (xml \\ "ExcludeFirstandLastQuote").headOption match {
          case Some(e) => e.child.text.toBoolean
          case None => false
        }

        val eventDate = getEventDate(xml, givenEventDate)
        val weekStarts = getWeekStarts(xml)
        val numberOfPricingPeriods = getNumberOfPricingPeriods(xml)
        val pricingPeriodType = getPricingPeriodType(xml)
        val countingStart = CountingStartOption.parse((xml \\ "CountingStart").head.child.text).get
        val countEventAsDay = (xml \\ "CountEventAsDay").head.child.text.toInt
        val numberOfCountingPeriods = (xml \\ "NumberOfCountingPeriods").head.child.text.toInt
        val countingPeriodType = PeriodType.parse((xml \\ "CountingPeriodType").head.child.text).get
        val countDirection = Direction.parse((xml \\ "CountDirection").head.child.text).get
        val pricingDirection = Direction.parse((xml \\ "PricingDirection").head.child.text).get
        val includeEventDate = (xml \\ "IncludeEventDate").head.child.text.toBoolean
        val applyEventDateTreatmentToFirstPricingDay = (xml \\ "ApplyEventDateTreatmentToFirstPricingDay").head.child.text.toBoolean
        val pricingPerCalendarPeriod = (xml \\ "PricingPerCalendarPeriod").map {
          case <PricingPerCalendarPeriod>{PricingPerCalendarPeriodOption(option)}</PricingPerCalendarPeriod> => option
        }.headOption match {
          case Some(p) => p
          case None => NoPricingPeriod
        }

        new ContinousPricingScheduleAlogorithm(numberOfPricingPeriods, pricingPeriodType, weekStarts, eventDateTreatment,
          excludeFirstandLastQuote, eventDate, nonPricingDayTreatment, countingStart, countEventAsDay, numberOfCountingPeriods,
          countingPeriodType, countDirection, pricingDirection, includeEventDate, applyEventDateTreatmentToFirstPricingDay, pricingPerCalendarPeriod)
      }
      case "AroundPricingScheduleAlgorithm" => {
        val nonPricingDayTreatment = getNonPricingDayTreatment(xml)
        val eventDateTreatment = getEventDateTreatment(xml)
        val numberOfPricingPeriods = getNumberOfPricingPeriods(xml)
        val pricingPeriodType = getPricingPeriodType(xml)
        val weekStarts = getWeekStarts(xml)
        val eventDate = getEventDate(xml, givenEventDate)

        new AroundPricingScheduleAlgorithm(numberOfPricingPeriods, pricingPeriodType, weekStarts, eventDateTreatment, eventDate, nonPricingDayTreatment)
      }
    }

    new PricingSchedule(algorithm, description, eventDateType, endEventDateType, parcelPricing)
  }

  private def getNumberOfPricingPeriods(xml: Node) = {
    (xml \\ "NumberOfPricingPeriods").head.child.text.toInt
  }

  private def getPricingPeriodType(xml: Node) = {
    PeriodType.parse((xml \\ "PricingPeriodType").head.child.text).get
  }

  private def getWeekStarts(xml: Node) = {
    (xml \\ "WeekStarts").headOption match {
      case Some(e) => DayOfWeek.parse(e.child.text)
      case None => DayOfWeek.monday
    }
  }

  private def getEventDate(xml: Node, givenEventDate: Option[Day]) = {
    (xml \\ "EventDate").headOption match {
      case Some(e) => {
        val day = Day.parse(e.child.text)
        if (givenEventDate.isDefined) {
          assert(givenEventDate.get == day)
        }
        day
      }
      case None => {
        assert(givenEventDate.isDefined)
        givenEventDate.get
      }
    }
  }

  private def getNonPricingDayTreatment(xml: Node) = {
    var nonPricingDayTreatment = new NonPricingDayTreatment
    (xml \\ "NonPricingDayTreatment").head.child map {
      case <Friday>{PricingRuleOption(option)}</Friday> => nonPricingDayTreatment = nonPricingDayTreatment.copy(friday = option)
      case <Saturday>{PricingRuleOption(option)}</Saturday> => nonPricingDayTreatment = nonPricingDayTreatment.copy(saturday = option)
      case <Sunday>{PricingRuleOption(option)}</Sunday> => nonPricingDayTreatment = nonPricingDayTreatment.copy(sunday = option)
      case <Monday>{PricingRuleOption(option)}</Monday> => nonPricingDayTreatment = nonPricingDayTreatment.copy(monday = option)
      case <Other>{PricingRuleOption(option)}</Other> => nonPricingDayTreatment = nonPricingDayTreatment.copy(other = option)
      case s => throw new Exception("Error parsing:" + s)
    }
    nonPricingDayTreatment
  }

  private def getEventDateTreatment(xml: Node) = {
    var eventDateTreatment = new EventDateTreatment
    (xml \\ "EventDateTreatment").head.child map {
      case <Friday>{EventRuleOption(option)}</Friday> => eventDateTreatment = eventDateTreatment.copy(friday = option)
      case <Saturday>{EventRuleOption(option)}</Saturday> => eventDateTreatment = eventDateTreatment.copy(saturday = option)
      case <Sunday>{EventRuleOption(option)}</Sunday> => eventDateTreatment = eventDateTreatment.copy(sunday = option)
      case <Monday>{EventRuleOption(option)}</Monday> => eventDateTreatment = eventDateTreatment.copy(monday = option)
      case <Other>{EventRuleOption(option)}</Other> => eventDateTreatment = eventDateTreatment.copy(other = option)
      case s => throw new Exception("Error parsing:" + s)
    }
    eventDateTreatment
  }

}