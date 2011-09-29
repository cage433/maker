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
case class AreaMaterialBenchmarkAtomicKey(
  area:Area, material:Material,
  override val ignoreShiftsIfPermitted : Boolean = false
) 
  extends AtomicDatumKey(AreaMaterialBenchmarkCurveKey(material.commodity), (area.code, material.grade.code), ignoreShiftsIfPermitted)
{
  def periodKey : Option[Period] = None
  def nullValue = Quantity(0.0, material.commodity.representativeMarket.priceUOM)
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    originalAtomicEnv.apply(this)
  }
}

/**
 * Benchmark curve key for benchmark area data
 */
case class AreaMaterialBenchmarkCurveKey(commodity : Commodity) extends NonHistoricalCurveKey[GradeAreaBenchmarkData]{
  override def typeName = "Price"
  def marketDataKey = GradeAreaBenchmarkMarketDataKey(commodity)
  def underlying = commodity.toString + " Benchmark"
  def buildFromMarketData(marketDay : DayAndTime, marketData : GradeAreaBenchmarkData) : CurveObject = {
    AreaMaterialBenchmarkCurveObject(marketDay, marketData)
  }
}

/**
 * Benchmark Location Curve for benchmarks using grade and area location as keys
 */
case class AreaMaterialBenchmarkCurveObject(marketDayAndTime : DayAndTime, marketData : GradeAreaBenchmarkData) extends CurveObject {
  val marketDataMap = marketData.areaData.toMap.withDefaultValue(Quantity.NULL)
  type CurveValuesType = Quantity

  def apply(point : AnyRef) = point match {
    case (area: AreaCode, grade: GradeCode) => {
      val benchmark: Quantity = marketDataMap((grade, area))

      benchmark
    }
  }
}

case class CommodityCountryBenchmarkAtomicKey(commodity: Commodity, country: NeptuneCountry,
  override val ignoreShiftsIfPermitted: Boolean = false
)
  extends AtomicDatumKey(CommodityCountryBenchmarkCurveKey(commodity), country.code, ignoreShiftsIfPermitted)
{
  def periodKey : Option[Period] = None
  def nullValue = Quantity(0.0, commodity.representativeMarket.priceUOM)
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    originalAtomicEnv.apply(this)
  }
}

/**
 * Benchmark curve key for benchmark location data
 */
case class CommodityCountryBenchmarkCurveKey(commodity : Commodity) extends NonHistoricalCurveKey[CountryBenchmarkData]{
  override def typeName = "Price"
  def marketDataKey = CountryBenchmarkMarketDataKey(commodity)
  def underlying = commodity.toString + " Benchmark"
  def buildFromMarketData(marketDay : DayAndTime, marketData : CountryBenchmarkData) : CurveObject = {
    CommodityCountryBenchmarkCurveObject(marketDay, marketData)
  }
}


/**
 * Benchmark Location Curve for benchmarks using grade and location as keys
 */
case class CommodityCountryBenchmarkCurveObject(marketDayAndTime : DayAndTime, marketData : CountryBenchmarkData) extends CurveObject {
  val countryData = marketData.countryData.toMap.withDefaultValue(Quantity.NULL)
  type CurveValuesType = Quantity

  def apply(point : AnyRef) = point match {
    case country : NeptuneCountryCode => {
      val benchmark: Quantity = countryData(country)

      benchmark
    }
  }
}
