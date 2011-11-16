package starling.curves

import interestrate.DayCountActual365
import starling.marketdata.{ForwardRateDataKey, ForwardRateData}
import math._
import starling.daterange.{DateRange, Day, DayAndTime}
import starling.quantity.{UOM, Quantity}
import starling.utils.ImplicitConversions._
import collection.immutable.Map
import starling.marketdata.ForwardRateSource._
import starling.metals.datasources.LIBORFixing
import scalaz.Scalaz._

/** The parent of all DiscountCurve implementations.
 */
trait DiscountCurve extends CurveObject{
  type CurveValuesType = Quantity
  def discount(day : Day) : Double
	
  /** Converts the generic apply method into a call to the implementation's
   * 	discount function.
   */
  def apply(point : AnyRef) : Quantity = {
	  new Quantity(discount(point.asInstanceOf[Day]))
	}
}

/** Static functions used in bootstrapping a discount curve from raw data.
 */
object SimpleDiscountCurve{
  

 
	/** The interest rate data may be any combination of swap rates, deposit rates and futures yields. This
  * 	converts them into an equivalent continuously compounded rate.
  * 	@todo [01 Apr 2010] Find out what products futures  def toDSL = {
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
 * Builds discounts using a contiguous set of forward forward rates. These must
 * begin at the market day.
 * The assumption is the continuously compounded rate is equal to the forward rate for
 * any consecutive days in the same forward rate period. This allows us to determine 'relative' or 'forward; discount
 * rates between two consecutive days. The discount rate between the market day and any day in the future can be
 * calculated as a product of these daily discounts.
 */
class ForwardForwardDiscountCurve(
  ccy : UOM,
  val marketDayAndTime : DayAndTime,
  rates : Map[DateRange, Quantity]
)
  extends DiscountCurve
{
  require(rates.values.forall(rate => rate.isScalar || rate.isPercent),
    "Require scalar or percentage rates but have: " + rates.values.map(_.uom).toSet.mkString(", "))

  if (! rates.isEmpty){
    val periods = rates.keySet.toList.sortWith(_<_)
    val expectedFirstDay = marketDayAndTime
    require(periods(0).firstDay == marketDayAndTime.day, "Periods should begin on the market day")
    periods.zip(periods.tail).foreach {
      case (p1, p2) =>
        require(p1.lastDay == p2.firstDay, "Periods must join up first and last days, got " + (p1, p2))
    }
  }
  val zippedData = rates.keySet.toList.sortWith(_<_).map{
    p =>
      val time = DayCountActual365.factor(p)
      val rate = rates(p).in(UOM.SCALAR).get.value
      println(rates(p) + ", " + rates(p).in(UOM.SCALAR).get + ", " + rate)
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
  def marketDataKey = ForwardRateDataKey(ccy)

  def buildFromMarketData(marketDayAndTime: DayAndTime, forwardRateData: ForwardRateData): DiscountCurve = {
    val fixings = forwardRateData.rates(LIBOR).map { case (tenor, rate) => LIBORFixing(ccy, marketDayAndTime.day, tenor, rate) }
      .toList.sortWith(_ < _)

    println("maturityDay: " + marketDayAndTime.day)
    fixings.foreach { fixing => println("%s maturityDay: %s" % (fixing.tenor, fixing.maturityDay)) }

//    fixings.foldLeft(marketDayAndTime.day)((lastMaturityDay, fixing) => {
//      println("Maturity Range: " + (lastMaturityDay upto fixing.maturityDay))
//      fixing.maturityDay
//    })

    val (_, forwardForwardRates) = fixings.foldLeft((marketDayAndTime.day, Map.empty[DateRange, Quantity])) {
      case ((lastMaturityDay, currentForwardForwardRates), fixing) => (fixing.maturityDay,
        currentForwardForwardRates.updated(lastMaturityDay upto fixing.maturityDay,
          fixing.forwardRate(lastMaturityDay, marketDayAndTime, currentForwardForwardRates)))
    }

    new ForwardForwardDiscountCurve(ccy, marketDayAndTime, forwardForwardRates)
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
    originalAtomicEnv.quantity(this) / originalAtomicEnv.quantity(copy(forwardDate = forwardDayAndTime.day))
  }
  override def clearProperties : AtomicDatumKey = copy(ignoreShiftsIfPermitted = false)
  def nullValue = new Quantity(0.9)
}

//object DiscountRateKey{
//  def apply(ccy : UOM, forwardDate : Day, props : String*) : DiscountRateKey = DiscountRateKey(ccy, forwardDate, Set[String]() ++ props)
//}

