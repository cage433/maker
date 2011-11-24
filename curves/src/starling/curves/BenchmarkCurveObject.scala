package starling.curves

import starling.market.Commodity
import starling.quantity.Quantity
import starling.marketdata._
import starling.utils.ImplicitConversions._
import starling.daterange.{Day, Period, DayAndTime}

/**
 * Class for atomic benchmark data
 */
case class AreaBenchmarkAtomicKey(area: AreaCode, commodity: Commodity, grade: GradeCode, day : Day,
  override val ignoreShiftsIfPermitted : Boolean = false
) 
  extends AtomicDatumKey(AreaBenchmarkCurveKey(commodity), (area, grade, day), ignoreShiftsIfPermitted)
{
  def periodKey : Option[Period] = None
  def nullValue = Quantity(0.0, commodity.representativeMarket.priceUOM)
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    originalAtomicEnv.apply(this)
  }
}

/**
 * Benchmark curve key for benchmark area data
 */
case class AreaBenchmarkCurveKey(commodity : Commodity) extends NonHistoricalCurveKey[GradeAreaBenchmarkData]{
  override def typeName = PriceDataType.name
  def marketDataKey = GradeAreaBenchmarkMarketDataKey(commodity)
  def underlying = commodity.toString + " Benchmark"
  def buildFromMarketData(marketDay : DayAndTime, marketData : GradeAreaBenchmarkData) : CurveObject = {
    AreaBenchmarkCurveObject(marketDay, marketData)
  }
}

/**
 * Benchmark Location Curve for benchmarks using grade and area location as keys
 */
case class AreaBenchmarkCurveObject(marketDayAndTime : DayAndTime, marketData : GradeAreaBenchmarkData) extends CurveObject {
  val marketDataMap = marketData.areaData.withDefaultValue(Map.empty[Day, Quantity])
  type CurveValuesType = Quantity

  def apply(point : AnyRef) = point match {
    case (area: AreaCode, grade: GradeCode, day: Day) => {
      val (days, benchmarks) = marketDataMap((grade, area)).sorted.unzip

      InverseConstantInterpolation.interpolate(days.toArray, benchmarks.toArray, day)
    }
  }
}

case class CountryBenchmarkAtomicKey(commodity: Commodity, country: NeptuneCountryCode, grade : GradeCode, day: Day, refData : ReferenceDataLookup,
  override val ignoreShiftsIfPermitted: Boolean = false
)
  extends AtomicDatumKey(CountryBenchmarkCurveKey(commodity, refData), (country, grade, day), ignoreShiftsIfPermitted)
{
  def periodKey : Option[Period] = None
  def nullValue = Quantity(0.0, commodity.representativeMarket.priceUOM)
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = try {
    originalAtomicEnv.apply(this)
  } catch {
    case e: MissingMarketDataException => throw new MissingMarketDataException(e.shortMessage + ", commodity: " + commodity,
      e.longMessage.map(_ + ", commodity: " + commodity))
  }
}

/**
 * Benchmark curve key for benchmark location data
 */
case class CountryBenchmarkCurveKey(commodity : Commodity, refData : ReferenceDataLookup) extends NonHistoricalCurveKey[CountryBenchmarkData]{
  override def typeName = PriceDataType.name
  def marketDataKey = CountryBenchmarkMarketDataKey(commodity)
  def underlying = commodity.toString + " Benchmark"
  def buildFromMarketData(marketDay : DayAndTime, marketData : CountryBenchmarkData) : CurveObject = {
    CountryBenchmarkCurveObject(marketDay, marketData, refData)
  }
}


/**
 * Benchmark Location Curve for benchmarks using grade and location as keys
 */
case class CountryBenchmarkCurveObject(marketDayAndTime : DayAndTime, marketData : CountryBenchmarkData, refData : ReferenceDataLookup) extends CurveObject {
  val countryData = marketData.countryData.withDefaultValue(Map.empty[Day, Quantity])
  type CurveValuesType = Quantity

  def apply(point : AnyRef) = point match {
    case (country: NeptuneCountryCode, grade : GradeCode, day: Day) => {
      val (days, benchmarks) = countryData((country, grade)).sorted.unzip
      if (benchmarks.isEmpty)
        throw new MissingMarketDataException("No benchmarks for country " + refData.countryFor(country) + ", grade code " + refData.gradeFor(grade),
          long = "No benchmarks for country %s (%s), grade code %s (%s)" % (refData.countryFor(country), country, refData.gradeFor(grade), grade))
      InverseConstantInterpolation.interpolate(days.toArray, benchmarks.toArray, day)
    }
  }
}
