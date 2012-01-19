package starling.curves

import starling.quantity.Quantity
import starling.marketdata._
import starling.utils.ImplicitConversions._
import collection.immutable.Map
import starling.daterange._
import starling.market._

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
  def buildFromMarketData(marketDay : DayAndTime, marketData : GradeAreaBenchmarkData, refData : ReferenceDataLookup) : CurveObject = {
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

case class CountryBenchmarkAtomicKey(commodity: Commodity, country: NeptuneCountryCode, grade : GradeCode, tenor:Tenor,
  override val ignoreShiftsIfPermitted: Boolean = false
)
  extends AtomicDatumKey(CountryBenchmarkCurveKey(commodity), (country, grade, tenor), ignoreShiftsIfPermitted)
{
  def periodKey : Option[Period] = None
  def nullValue = Quantity(0.0, commodity.representativeMarket.priceUOM)
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = try {
    originalAtomicEnv.apply(this)
  } catch {
    case e: MissingMarketDataException => Quantity.NULL  // Hack until UAT is over
//      throw new MissingMarketDataException(e.shortMessage + ", commodity: " + commodity,
//      e.longMessage.map(_ + ", commodity: " + commodity))
  }
}

/**
 * Benchmark curve key for benchmark location data
 */
case class CountryBenchmarkCurveKey(commodity : Commodity) extends NonHistoricalCurveKey[CountryBenchmarkData]{
  override def typeName = PriceDataType.name
  def marketDataKey = CountryBenchmarkMarketDataKey(commodity)
  def underlying = commodity.toString + " Benchmark"
  def buildFromMarketData(marketDay : DayAndTime, marketData : CountryBenchmarkData, refData : ReferenceDataLookup) : CurveObject = {
    CountryBenchmarkCurveObject(marketDay, marketData, refData)
  }
}


/**
 * Benchmark Location Curve for benchmarks using grade and location as keys
 */
case class CountryBenchmarkCurveObject(marketDayAndTime : DayAndTime,
                                       marketData : CountryBenchmarkData,
                                       refData : ReferenceDataLookup) extends CurveObject {
  val countryData = marketData.countryData.withDefaultValue(Map.empty[Tenor, Quantity])
  type CurveValuesType = Quantity

  def apply(point : AnyRef) = point match {
    case (country: NeptuneCountryCode, grade : GradeCode, tenor: Tenor) => {
      try {
        countryData((country, grade)).keys.toList.sortWith(_ > _).find(_ <= tenor) match {
          case None =>
            throw new MissingMarketDataException("No benchmarks for country " + refData.countryFor(country) + ", grade code " + refData.gradeFor(grade),
              long = "No benchmark for country %s (%s), grade code %s (%s), day %s" %(refData.countryFor(country), country, refData.gradeFor(grade), grade, tenor))
          case Some(tenor) => countryData((country, grade))(tenor)
        }
      } catch {
        case _ : MissingMarketDataException => Quantity.NULL
      }
    }
  }
}
