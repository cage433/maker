package starling.guiapiextras

import starling.daterange.{DateRangePeriod, SpreadPeriod, Period, DateRange}
import starling.quantity.{UOM, Quantity}

case class UTPType(
  instrumentTypeName : String,
  volumeUOM : UOM,
  period : Option[DateRange],
  secondPeriod : Option[DateRange] = None,
  callPut : Option[String] = None,
  strike : Option[Quantity] = None,
  marketName : String,
  commodityName : String = "",
  isNaturallyHedgedWithCommoditySwap : Boolean = false
 ) extends Ordered[UTPType]{

  def spreadPeriod : Option[Period] = (period, secondPeriod) match {
    case (Some(p1), Some(p2)) => Some(SpreadPeriod(p1, p2))
    case (Some(p1), None) => Some(DateRangePeriod(p1))
    case (None, None) => None
    case _ => throw new Exception("Illegal period combination, first = " + period + ", second = " + secondPeriod)
  }
  override def toString = {
    List(Some(instrumentTypeName), callPut, strike).flatMap{x => x}.mkString(" ")
  }
  def compare(that: UTPType) = {
    if (instrumentTypeName != that.instrumentTypeName)
      instrumentTypeName.compare(that.instrumentTypeName)
    else if (period != that.period)
      period.get.compare(that.period.get)
    else if (secondPeriod != that.secondPeriod)
      secondPeriod.get.compare(that.secondPeriod.get)
    else if (callPut != that.callPut)
      callPut.get.compare(that.callPut.get)
    else if (strike != that.strike)
      strike.get.compare(that.strike.get)
    else
      0
  }
}