package starling.curves

import starling.pivot._
import starling.pivot.model.PivotTableModel
import controller.PivotTable
import starling.utils.cache.CacheFactory
import collection.immutable.List
import starling.db.{MarketDataReader, NormalMarketDataReader, MarketDataStore}
import starling.gui.api.{EnvironmentSpecificationLabel, CurveLabel, CurveTypeLabel, MarketDataIdentifier}
import starling.quantity.UOM
import starling.marketdata.{CountryBenchmarkData, CountryBenchmarkMarketDataKey, ForwardRateData}

case class CurveSpecification(curveType: CurveType, marketDataIdentifier: MarketDataIdentifier,
                              environmentSpecification: EnvironmentSpecification)

class CurveViewer(marketDataStore : MarketDataStore, environmentRules: EnvironmentRules) {
  val curveTypeLookup = Map(CurveTypeLabel("Price") → PriceCurveFactory,
                            CurveTypeLabel("Implied Vol") -> ImpliedVolCurveTypeFactory,
                            CurveTypeLabel("Swap Vol") -> SwapVolScheduleFactory,
                            CurveTypeLabel("Discount") -> DiscountCurveType,
                            CurveTypeLabel("Forward FX") -> ForwardFXCurveType,
                            CurveTypeLabel("Benchmarks") -> BenchmarkCurveType
  )
  val curveTypes = curveTypeLookup.keys.toList

  val curveCache = CacheFactory.getCache("CurveViewer.Curve")
  val tableCache = CacheFactory.getCache("CurveViewer.PivotTable")

  def curve(curveLabel: CurveLabel): PivotTableDataSource = curveCache.memoize(curveLabel, createPivot(curveLabel))

  def pricePivotTableFor(marketDataIdentifier: MarketDataIdentifier, envSpec: EnvironmentSpecificationLabel,
                   fieldsState: PivotFieldsState): PivotTable = {

    tableCache.memoize((marketDataIdentifier, envSpec, fieldsState),
      PivotTableModel.createPivotTableData(curve(CurveLabel(CurveTypeLabel("Price"), marketDataIdentifier, envSpec)), fieldsState))
  }

  private def createPivot(curveLabel: CurveLabel): PivotTableDataSource = {
    val curveType: CurveType = curveTypeLookup(curveLabel.curveType)
    val reader = new NormalMarketDataReader(marketDataStore, curveLabel.marketDataIdentifier)
    curveType.create(reader, forLabel(curveLabel.environmentSpecification))
  }

  private def forLabel(label: EnvironmentSpecificationLabel) =
    new EnvironmentSpecification(label.observationDay, environmentRules.forLabel(label.environmentRule))
}

trait CurveType {
  def create(reader:MarketDataReader, envSpec: EnvironmentSpecification):PivotTableDataSource
}

trait EnvironmentWithDomain {
  def environment: Environment
  def markets:List[UnderlyingDeliveryPeriods]
  //These should not really have default Nil implementations as the Curve viewer shows nothing if this is the case
  //However, at the moment we don't have a good definitions of the EnvironmentRules
  def marketVols: List[MarketOptionData] = Nil
  def discounts:List[(UOM,ForwardRateData)] = Nil
  def spotFX:List[UOM] = Nil
  def benchmarks:List[(CountryBenchmarkMarketDataKey,CountryBenchmarkData)] = Nil
}
