package starling.rmi

import starling.daterange.Day
import starling.gui.api._
import starling.pivot.model.PivotTableModel
import starling.marketdata._
import starling.db._
import starling.utils.ImplicitConversions._
import starling.manager.BromptonContext
import starling.curves.{EnvironmentRules, CurveViewer}
import starling.utils.cache.CacheFactory
import starling.pivot.{PivotTableDataSource, NullPivotTableDataSource, PivotFieldParams, PivotEdits}
import starling.fc2.api.{FC2InitialData, FC2Facility}


trait MarketDataPageIdentifierReaderProvider {
  def readerFor(identifier:MarketDataPageIdentifier):Option[MarketDataReader]
}

trait MarketDataPageIdentifierReaderProviders {
  def providers:List[MarketDataPageIdentifierReaderProvider]
}
object MarketDataPageIdentifierReaderProviders {
  val Empty = new MarketDataPageIdentifierReaderProviders {
    def providers = Nil
  }
}

class BromptonTrackerBasedMarketDataPageIdentifierReaderProviders(context:BromptonContext) extends MarketDataPageIdentifierReaderProviders {
  val tracker = context.createServiceTracker[MarketDataPageIdentifierReaderProvider](Some(classOf[MarketDataPageIdentifierReaderProvider]))
  def providers = tracker.flatMap( s => List(s) ).toList
}

class FC2Service(marketDataStore: MarketDataStore, marketDataTypes: MarketDataTypes, marketDataReaderProviders:MarketDataPageIdentifierReaderProviders) {
  private val cache = CacheFactory.getCache("FC2")

  def latest(marketDataSelection:MarketDataSelection) = marketDataStore.latest(marketDataSelection)

  def marketDataPivot(selection:MarketDataSelection, marketDataType:MarketDataType):PivotTableDataSource = {
    marketDataPivot(marketDataStore.latestMarketDataIdentifier(selection), marketDataType)
  }

  def marketDataPivot(marketDataIdentifier:MarketDataIdentifier, marketDataType:MarketDataType):PivotTableDataSource = {
    marketDataPivot(StandardMarketDataPageIdentifier(marketDataIdentifier), marketDataType)
  }

  def marketDataPivot(marketDataPageIdentifier:MarketDataPageIdentifier, marketDataType:MarketDataType):PivotTableDataSource = {
    marketDataSource(marketDataPageIdentifier, Some(MarketDataTypeLabel(marketDataType.name.name)), PivotEdits.Null)
  }

  def marketDataSource(marketDataPageIdentifier:MarketDataPageIdentifier, marketDataTypeLabel:Option[MarketDataTypeLabel], edits:PivotEdits) = {
    val preBuilt = cache.memoize( (marketDataPageIdentifier, marketDataTypeLabel), {
      if (marketDataPageIdentifier.selection.isNull) None else {
        val reader = marketDataReaderFor(marketDataPageIdentifier)
        val marketDataType = marketDataTypeLabel match {
          case None => PriceDataType
          case Some(mdt) => realTypeFor(mdt)
        }
        Some(new PrebuiltMarketDataPivotData(reader, marketDataStore,
          marketDataPageIdentifier.marketDataIdentifier, marketDataType, marketDataTypes))
      }
    })
    preBuilt match {
      case Some(p) => new MarketDataPivotTableDataSource(p, edits)
      case None => NullPivotTableDataSource
    }
  }

  def marketDataReaderFor(marketDataIdentifier:MarketDataPageIdentifier) = {
    val reader = marketDataIdentifier match {
      case StandardMarketDataPageIdentifier(mdi) => new NormalMarketDataReader(marketDataStore, mdi)
      case _ => {
        val readers = marketDataReaderProviders.providers.flatMap( provider => {
          provider.readerFor(marketDataIdentifier)
        })
        readers match {
          case Nil => throw new Exception("No market data reader found for " + marketDataIdentifier)
          case one :: Nil => one
          case _ => throw new Exception("More than one market data reader found for " + marketDataIdentifier)
        }
      }
    }

    reader

    // TODO: [18 Nov 2011]: Validation adds a massive overhead, reduce.
//    if (marketDataIdentifier.selection.pricingGroup == Some(PricingGroup.Metals)) {
//      reader // validate(reader)
//    } else {
//      reader
//    }
  }

  private def realTypeFor(label:MarketDataTypeLabel) = {
    marketDataTypes.types.find(_.toString == label.name).getOrElse(throw new Exception("Can't find market data type '" + label.name + "' in " + marketDataTypes.types))
  }

  private def validate(reader: MarketDataReader): MarketDataReader = {
    new ValidatingMarketDataReader(reader, RollingAveragePriceValidator, new DayChangePriceValidator(reader))
  }

  private def sortMarketDataTypes(types:List[MarketDataType]) = types.sortWith(_.name.name < _.name.name)
}

class FC2FacilityImpl(service: FC2Service,
                      marketDataStore:MarketDataStore,
                      curveViewer : CurveViewer,
                      environmentRules: EnvironmentRules,
                      enabledDesks: Set[Desk]) extends FC2Facility {


  val cache = CacheFactory.getCache("FC2")

  private def excelLatestMarketDataVersions = marketDataStore.latestExcelVersions.mapKeys(_.stripExcel)
  private def pricingGroupLatestMarketDataVersions = Map() ++ marketDataStore.latestPricingGroupVersions.filterKeys(pricingGroups()).toMap

  private def pricingGroupDefinitions() = {
    val allPricingGroups = enabledDesks.flatMap(_.pricingGroups).toSet
    marketDataStore.pricingGroupDefinitions.filter(d=>allPricingGroups.contains(d.pricingGroup))
  }
  private def pricingGroups() = pricingGroupDefinitions.map(_.pricingGroup)

  private def excelDataSets() = marketDataStore.excelDataSets

  private def environmentRuleLabels = pricingGroups.toMapWithValues(environmentRules.forPricingGroup(_).map(_.label))

  private val curveTypes = curveViewer.curveTypes

  private def snapshots():Map[MarketDataSelection,List[SnapshotIDLabel]] = {
    marketDataStore.snapshotsByMarketDataSelection
  }

  private def observationDays():(Map[PricingGroup,Set[Day]],Map[String,Set[Day]]) = {
    (Map() ++ marketDataStore.observationDaysByPricingGroup(), Map() ++ marketDataStore.observationDaysByExcel())
  }

  def init() = FC2InitialData(
    snapshots(), observationDays(), pricingGroupDefinitions(), environmentRuleLabels,
    excelDataSets(), excelLatestMarketDataVersions, pricingGroupLatestMarketDataVersions, curveTypes)

  def curvePivot(curveLabel: CurveLabel, pivotFieldParams: PivotFieldParams) = {
    PivotTableModel.createPivotData(curveViewer.curve(curveLabel), pivotFieldParams)
  }

  def readAllMarketData(marketDataIdentifier:MarketDataPageIdentifier, marketDataTypeLabel:Option[MarketDataTypeLabel], edits:PivotEdits, pivotFieldParams:PivotFieldParams):PivotData = {
    val dataSource = service.marketDataSource(marketDataIdentifier, marketDataTypeLabel, edits)
    PivotTableModel.createPivotData(dataSource, pivotFieldParams)
  }

  def saveMarketData(marketDataIdentifier:MarketDataPageIdentifier, marketDataTypeLabel:Option[MarketDataTypeLabel], pivotEdits:PivotEdits) = {
    val dataSource = service.marketDataSource(marketDataIdentifier, marketDataTypeLabel, PivotEdits.Null)
    dataSource.editable.get.save(pivotEdits)
  }

  def snapshot(marketDataIdentifier: MarketDataIdentifier, snapshotType: SnapshotType) = {
    marketDataStore.snapshot(marketDataIdentifier, snapshotType).label
  }

  def importData(marketDataSelection:MarketDataSelection, observationDay:Day) = {
    val saveResult = marketDataStore.importData(marketDataSelection, observationDay)
    SpecificMarketDataVersion(saveResult.maxVersion)
  }

  def latestMarketDataIdentifier(selection:MarketDataSelection):MarketDataIdentifier = marketDataStore.latestMarketDataIdentifier(selection)

  private def label(snapshot:SnapshotID):SnapshotIDLabel = snapshot.label
  private def sortMarketDataTypes(types:List[MarketDataType]) = types.sortWith(_.name.name < _.name.name)
}