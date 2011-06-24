package starling.curves

import starling.market.Commodity
import starling.daterange.Period
import starling.quantity.Quantity
import starling.daterange.DayAndTime
import starling.marketdata._
import starling.utils.ImplicitConversions._

/**
 * Class for atomic benchmark data
 */
case class BenchmarkAtomicKey(
  commodity : Commodity, 
  hub : Hub,
  grade : Grade, 
  override val ignoreShiftsIfPermitted : Boolean = false
) 
  extends AtomicDatumKey(BenchmarkHubAndGradeCurveKey(commodity), (hub, grade), ignoreShiftsIfPermitted)
{
  def periodKey : Option[Period] = None
  def nullValue = Quantity(0.0, commodity.representativeMarket.priceUOM)
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    originalAtomicEnv.apply(this)
  }
}

/**
 * Benchmark curve key for benchmark hub data
 */
case class BenchmarkHubAndGradeCurveKey(commodity : Commodity) extends NonHistoricalCurveKey[GradeHubBenchmarkData]{
  override def typeName = "Price"
  def marketDataKey = GradeHubBenchmarkMarketDataKey(commodity)
  def underlying = commodity.toString + " Benchmark"
  def buildFromMarketData(marketDay : DayAndTime, marketData : GradeHubBenchmarkData) : CurveObject = {
    BenchmarkHubCurveObject(marketDay, marketData)
  }
}

/**
 * Benchmark curve key for benchmark location data
 */
case class BenchmarkLocationCurveKey(commodity : Commodity) extends NonHistoricalCurveKey[BenchmarkLocationData]{
  override def typeName = "Price"
  def marketDataKey = BenchmarkLocationMarketDataKey(commodity)
  def underlying = commodity.toString + " Benchmark"
  def buildFromMarketData(marketDay : DayAndTime, marketData : BenchmarkLocationData) : CurveObject = {
    BenchmarkLocationCurveObject(marketDay, marketData)
  }
}

/**
 * Benchmark Location Curve for benchmarks using grade and hub location as keys
 */
case class BenchmarkHubCurveObject(marketDayAndTime : DayAndTime, marketData : GradeHubBenchmarkData) extends CurveObject {
  val marketDataMap = marketData.hubData.toMap

  type CurveValuesType = Quantity

  def apply(point : AnyRef) = {
    point match {
      case (hub:Hub, grade : Grade) => {
        val hubBenchmark  : Quantity = marketDataMap((grade, hub))
        hubBenchmark 
      }
    }
  }
}

/**
 * Benchmark Location Curve for benchmarks using grade and location as keys
 */
case class BenchmarkLocationCurveObject(marketDayAndTime : DayAndTime, marketData : BenchmarkLocationData) extends CurveObject {

  type CurveValuesType = Quantity

  def apply(point : AnyRef) = {
    point match {
      case location : Location => {
        val hubBenchmark  : Quantity = marketData.locationData(location)
        hubBenchmark
      }
    }
  }
}
