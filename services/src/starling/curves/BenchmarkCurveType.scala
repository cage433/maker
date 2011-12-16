package starling.curves

import starling.db.MarketDataReader
import starling.pivot.FieldDetails._
import starling.pivot.FieldDetailsGroup._
import starling.pivot._
import starling.daterange.{Tenor, Month}
import starling.instrument.physical.PhysicalMetalAssignmentOrUnassignedSalesQuota
import starling.market.{Market, FuturesMarket, FuturesFrontPeriodIndex, FuturesExchangeFactory}

object BenchmarkCurveType extends CurveType {
  def create(reader: MarketDataReader, envSpec: EnvironmentSpecification) = {
    val context = envSpec.environmentRule.createEnv(envSpec.observationDay, reader)
    new BenchmarkPivotTableDataSource(context)
  }
}

class BenchmarkPivotTableDataSource(context:EnvironmentWithDomain) extends UnfilteredPivotTableDataSource {
  val commodityField = FieldDetails("Commodity")
  val countryField = FieldDetails("Country")
  val gradeField = FieldDetails("Grade")
  val tenorField = FieldDetails("Month offset")
  val monthField = FieldDetails("Month")
  val shanghaiField = FieldDetails("Shanghai/Normal")
  val benchmarkField = new PivotQuantityFieldDetails("Benchmark")
  val fieldDetailsGroups = List(FieldDetailsGroup("Benchmarks", commodityField, countryField, gradeField, monthField, tenorField, shanghaiField, benchmarkField))


  override def initialState = DefaultPivotState(PivotFieldsState(
    rowFields = List(commodityField, countryField, gradeField, shanghaiField).map(_.field),
    columnFields = List(monthField, tenorField).map(_.field),
    dataFields = benchmarkField.field :: Nil
  ))

  val rows = {
    val marketDay = context.environment.marketDay.day
    val currentMonth = marketDay.containingMonth
    val months = (currentMonth upto currentMonth + 12)
    val exchanges = List(
      ("Shanghai",FuturesFrontPeriodIndex(Market.SHANGHAI_COPPER)),
      ("LME/COMEX/Wuxi", FuturesFrontPeriodIndex(Market.LME_COPPER))
    )
    context.benchmarks.flatMap { case (key,data) => {
      exchanges.flatMap { case (indexLabel, index) => {
        data.countryData.keys.flatMap { case (country,grade) => {
          months.flatMap { month => {
            val tenor = PhysicalMetalAssignmentOrUnassignedSalesQuota.benchmarkTenor(index, context.environment.marketDay, month)
            if (tenor.value > 0) {
              val benchmark = context.environment.benchmarkPremium(key.commodity, country, grade, tenor)
              fields(commodityField -> key.commodity.name, countryField -> country.code, gradeField -> grade.code,
                monthField -> month, tenorField -> tenor, benchmarkField -> benchmark.pq, shanghaiField -> indexLabel) :: Nil
            } else Nil
          } }
        } }
      }}
    }}
  }



  def unfilteredData(pfs: PivotFieldsState) = rows
}