package starling.curves

import interestrate.{DayCountActual365, DayCountActualActual}
import starling.marketdata.{ForwardRateDataKey, ForwardRateDataEntry, ForwardRateData}
import starling.varcalculator.RiskFactor
import math._
import starling.daterange.{DateRange, Day, DayAndTime}
import starling.quantity.{Percentage, UOM, Quantity}
import starling.quantity.Quantity._
import starling.utils.ImplicitConversions._

/** The parent of all DiscountCurve implementations. 
 */
trait DiscountCurve extends CurveObject{
  type CurveValuesType = Double
  def discount(day : Day) : Double
	
  /** Converts the generic apply method into a call to the implementation's
   * 	discount function.
   */
  def apply(point : AnyRef) : Double = {
	  discount(point.asInstanceOf[Day])
	}
}

/** Static functions used in bootstrapping a discount curve from raw data.
 */
object SimpleDiscountCurve{
  

 
	/** The interest rate data may be any combination of swap rates, deposit rates and futures yields. This
  * 	converts them into an equivalent continuously compounded rate.
  * 	@todo - Find out what products futures  def toDSL = {
    com.trafigura.tradinghub.model.LinearInterpolationMethod
  }
 yields represent. From the data it seems clear that they represent
  * 					a yield, but IIRC futures interest rate products are forward forward rates.
  */
	def convertRateToCCZeroRate(marketDay : Day, forwardDay : Day, format : String, typeName : String, rate : Double) : Double = {
    val time = forwardDay.daysSinceInYears(marketDay)
    if (time == 0)
      rate
    else
      (format, typeName) match {
        case (_, "DEPO") => rate
        case ("Yield", "FUTURE") => 1.0 - rate
        case ("Quarterly", "SWAP") =>
          val nQuarters = time * 4.0
          log(pow(1.0 + rate /  4.0, nQuarters)) / time
        case ("Semi-Ann", "SWAP") =>
          val nHalfYears = time * 2.0
          log(pow(1.0 + rate / 2.0, nHalfYears)) / time
        case("Discount", _) => -log(rate) / time
        case ("Annual", "SWAP") =>
          val nYears = time
          log(pow(1.0 + rate, nYears)) / time
      }
  }
}

/** Used purely in unit tests. It can often be convenient to use curves with easily
 * 	understood characteristics.
 */
case class ConstantDiscountCurve(
  marketDayAndTime : DayAndTime, 
  ccy : UOM, 
  z : Double
)
	extends DiscountCurve  
{
  def discount(day : Day) : Double = {
    val t = day.daysSinceInYears(marketDay)
    math.exp(- z * t)
  }
  
}

/** The default discount curve. It has a set of zero rates for some days. Linear interpolation
 * 	is done to determine zero rates between these days, extrapolation beyond the last is done by
 * 	keeping the curve constant.
 */
case class SimpleDiscountCurve(
  ccy : UOM,
  marketDayAndTime : DayAndTime,
  days : Array[Day], 
  ccZeroRates : Array[Double]
)
	extends DiscountCurve
{

	require (days.size > 0, "No data with which to construct the discount curve for currency " + ccy)
	require (days.size == ccZeroRates.size, "Days and zero rates must have matching sizes")
  require (days.toList == days.toList.sortWith(_<_), "Days not in order")
  require (days.head >= marketDayAndTime.day, "First day must be on or after market day")

  
  val (firstDay, lastDay) = (days.head, days.last)

  def zeroRate (day : Day) : Double = {
  	if (day < marketDay){
  	  throw new IllegalStateException("Can't get zero rates for " + day + ", it is prior to market day " + marketDayAndTime.day)
  	} else if (day < firstDay){
  	  zeroRate(firstDay)
  	} else if (day > lastDay){
  	  zeroRate(lastDay)
  	} else {
  	  LinearInterpolation.interpolate(days, ccZeroRates, day)
  	}
  }
  
  def discount (day : Day) : Double = {
    if (day == marketDay)
      1.0
    else {
      val t = day.daysSinceInYears(marketDay)
      val z = zeroRate(day)
      exp(-z * t)
    }
  }
}


/**
 * An implementation of DiscountCurve using forward forward rates
 */
class ForwardForwardDiscountCurve(
  ccy : UOM,
  val marketDayAndTime : DayAndTime,
  rates : Map[DateRange, Percentage]
)
  extends DiscountCurve
{
  if (! rates.isEmpty){
    val periods = rates.keySet.toList.sortWith(_<_)
    val expectedFirstDay = marketDayAndTime
    assert(periods(0).firstDay == marketDayAndTime.day, "Periods should begin on the market day")
    periods.zip(periods.tail).foreach {
      case (p1, p2) =>
        assert(p1.lastDay == p2.firstDay, "Periods must join up first and last days, got " + (p1, p2))
    }
  }
  val zippedData = rates.keySet.toList.sortWith(_<_).map{
    p =>
      val time = DayCountActual365.factor(p)
      val rate = rates(p).value
      val z = math.log(1 + rate * time) / time
      (p, time, z)
  }
  def discount(day: Day) : Double = {
    if (day == marketDay)
      1.0
    else {
      if (rates.isEmpty)
        throw new MissingForwardFXException(ccy, day, "No forward rates supplied for currency " + ccy)
      var d = 1.0
      for ((p, t, z) <- zippedData; if p.firstDay < day) {
        val time = if (p.lastDay <= day) t else DayCountActual365.factor(p.firstDay, day)
        d *= math.exp(-z * time)
      }
      d
    }
  }
}



case class DiscountCurveKey(ccy : UOM) extends NonHistoricalCurveKey[ForwardRateData]{
  def build(marketDayAndTime : DayAndTime, riskFactorPrices: Map[RiskFactor, Quantity]) = throw new UnsupportedOperationException("Discounts are not currently risk factors")
  def marketDataKey = ForwardRateDataKey(ccy)
  def buildFromMarketData(marketDayAndTime: DayAndTime, forwardRateData: ForwardRateData):SimpleDiscountCurve = {
    // put the results in date order
    var results = forwardRateData.entries.sortWith(_.forwardDay < _.forwardDay)
    /* 	As the different products (futures, swaps, deposits) may trade at reasonably sized spreads to each other,
    * 	bootstrapping a curve from a combination can lead to unrealistic forward rates. To avoid this we look at
    * 	the product type at the latest point on the curve, and then remove data for all other product types. This
    * 	wouldn't be acceptable for an interest rate system, but should be 'good enough' for commodities.
    * 	@todo - check the above assumption is OK
    */
    var points = Map.empty[Day, Double]
    val lastType = results.last.trinityInstrumentType
    results.filter(_.trinityInstrumentType == lastType).foreach{
      case ForwardRateDataEntry(day, format, _, rate) => {
        val z = SimpleDiscountCurve.convertRateToCCZeroRate(marketDayAndTime.day, day, format, lastType, rate)
        points = points + (day -> z)
      }
    }
    val days = points.keySet.toList.sortWith(_<_).filter(_ >= marketDayAndTime.day)
    days match {
      case Nil => throw new MissingMarketDataException("No forward rate data for " + ccy)
      case _ => {
        val ccZeroRates = days.toArray.map(points)
          SimpleDiscountCurve(ccy, marketDayAndTime, days.toArray, ccZeroRates)
      }
    }
  }

  def underlying = ccy.toString
}

case class DiscountRateKey(
  ccy : UOM, 
  forwardDate : Day,
  override val ignoreShiftsIfPermitted : Boolean = false
)
  extends AtomicDatumKey(DiscountCurveKey(ccy), forwardDate, ignoreShiftsIfPermitted)
{
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    originalAtomicEnv.double(this) / originalAtomicEnv.double(copy(forwardDate = forwardDayAndTime.day))
  }
  override def clearProperties : AtomicDatumKey = copy(ignoreShiftsIfPermitted = false)
  def nullValue = 0.9

  def periodKey = Some(forwardDate)
}

//object DiscountRateKey{
//  def apply(ccy : UOM, forwardDate : Day, props : String*) : DiscountRateKey = DiscountRateKey(ccy, forwardDate, Set[String]() ++ props)
//}

