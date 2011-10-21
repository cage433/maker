package starling.rmi

import starling.daterange.Day
import starling.gui.api._
import starling.pivot.model.PivotTableModel
import starling.marketdata._
import starling.fc2.api.FC2Facility
import starling.db._
import starling.utils.ImplicitConversions._
import starling.manager.BromptonContext
import starling.curves.{EnvironmentRules, CurveViewer, EnvironmentRule}
import starling.utils.cache.CacheFactory
import starling.pivot.{PivotTableDataSource, NullPivotTableDataSource, PivotFieldParams, PivotEdits}


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

class FC2FacilityImpl(
                      marketDataStore:MarketDataStore,
                      curveViewer : CurveViewer,
                      marketDataReaderProviders:MarketDataPageIdentifierReaderProviders,
                      marketDataTypes: MarketDataTypes,
                      environmentRules: EnvironmentRules) extends FC2Facility {

  private def unLabel(pricingGroup:PricingGroup) = pricingGroup
  private def unLabel(snapshotID:SnapshotIDLabel) = marketDataStore.snapshotFromID(snapshotID.id).get
  private def label(pricingGroup:PricingGroup):PricingGroup = pricingGroup
  private def label(snapshot:SnapshotID):SnapshotIDLabel = snapshot.label

  val cache = CacheFactory.getCache("FC2")
  def pricingGroups = {
    //val allPricingGroups = desks.flatMap(_.pricingGroups).toSet
    marketDataStore.pricingGroups//.filter(allPricingGroups.contains(_))
  }
  def excelDataSets() = marketDataStore.excelDataSets

  def environmentRuleLabels = pricingGroups.toMapWithValues(environmentRules.forPricingGroup(_).map(_.label))

  val curveTypes = curveViewer.curveTypes

  def snapshots():Map[MarketDataSelection,List[SnapshotIDLabel]] = {
    marketDataStore.snapshotsByMarketDataSelection
  }

  def observationDays():(Map[PricingGroup,Set[Day]],Map[String,Set[Day]]) = {
    (Map() ++ marketDataStore.observationDaysByPricingGroup(), Map() ++ marketDataStore.observationDaysByExcel())
  }

  private def realTypeFor(label:MarketDataTypeLabel) = {
    marketDataTypes.types.find(_.toString == label.name).getOrElse(throw new Exception("Can't find market data type '" + label.name + "' in " + marketDataTypes.types))
  }

  def curvePivot(curveLabel: CurveLabel, pivotFieldParams: PivotFieldParams) = {
    PivotTableModel.createPivotData(curveViewer.curve(curveLabel), pivotFieldParams)
  }

  def marketDataPivot(selection:MarketDataSelection, marketDataType:MarketDataType):PivotTableDataSource = {
    marketDataPivot(marketDataStore.latestMarketDataIdentifier(selection), marketDataType)
  }

  def marketDataPivot(marketDataIdentifier:MarketDataIdentifier, marketDataType:MarketDataType):PivotTableDataSource = {
    marketDataPivot(StandardMarketDataPageIdentifier(marketDataIdentifier), marketDataType)
  }

  def marketDataPivot(marketDataPageIdentifier:MarketDataPageIdentifier, marketDataType:MarketDataType):PivotTableDataSource = {
    marketDataSource(marketDataPageIdentifier, Some(MarketDataTypeLabel(marketDataType.name.name)), PivotEdits.Null)
  }

  def latest(marketDataSelection:MarketDataSelection) = marketDataStore.latest(marketDataSelection)

  def marketDataSource(marketDataPageIdentifier:MarketDataPageIdentifier, marketDataTypeLabel:Option[MarketDataTypeLabel], edits:PivotEdits) = {
    val preBuilt = cache.memoize( (marketDataPageIdentifier, marketDataTypeLabel), {
      val reader = marketDataReaderFor(marketDataPageIdentifier)
      val marketDataType = marketDataTypeLabel match {
        case None => {
          sortMarketDataTypes(reader.marketDataTypes) match {
            case Nil => None
            case many => many.headOption
          }
        }
        case Some(mdt) => Some(realTypeFor(mdt))
      }
      marketDataType.map { mdt =>
        new PrebuiltMarketDataPivotData(reader, marketDataStore,
          marketDataPageIdentifier.marketDataIdentifier, mdt, marketDataTypes)
      }
    })
    preBuilt match {
      case Some(preBuilt) => new MarketDataPivotTableDataSource(preBuilt, edits)
      case None => NullPivotTableDataSource
    }
  }

  def readAllMarketData(marketDataIdentifier:MarketDataPageIdentifier, marketDataTypeLabel:Option[MarketDataTypeLabel], edits:PivotEdits, pivotFieldParams:PivotFieldParams):PivotData = {
    val dataSource = marketDataSource(marketDataIdentifier, marketDataTypeLabel, edits)
    PivotTableModel.createPivotData(dataSource, pivotFieldParams)
  }

  def saveMarketData(marketDataIdentifier:MarketDataPageIdentifier, marketDataTypeLabel:Option[MarketDataTypeLabel], pivotEdits:PivotEdits) = {
    val dataSource = marketDataSource(marketDataIdentifier, marketDataTypeLabel, PivotEdits.Null)
    dataSource.editable.get.save(pivotEdits)
  }

  def snapshot(marketDataIdentifier: MarketDataIdentifier, snapshotType: SnapshotType) = {
    marketDataStore.snapshot(marketDataIdentifier, snapshotType).label
  }

  def importData(marketDataSelection:MarketDataSelection, observationDay:Day) = {
    val saveResult = marketDataStore.importData(marketDataSelection, observationDay)
    SpecificMarketDataVersion(saveResult.maxVersion)
  }

  def excelLatestMarketDataVersions = marketDataStore.latestExcelVersions.mapKeys(_.stripExcel)
  def pricingGroupLatestMarketDataVersions = Map() ++ marketDataStore.latestPricingGroupVersions.filterKeys(pricingGroups()).toMap

  private def marketDataReaderFor(marketDataIdentifier:MarketDataPageIdentifier) = {
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
    validate(reader)
  }

  private def validate(reader: MarketDataReader): MarketDataReader = {
    new ValidatingMarketDataReader(reader, RollingAveragePriceValidator, new DayChangePriceValidator(reader))
  }

  def marketDataTypeLabels(marketDataIdentifier:MarketDataPageIdentifier) = {
    sortMarketDataTypes(marketDataReaderFor(marketDataIdentifier).marketDataTypes).map(t=>MarketDataTypeLabel(t.name.name))
  }

  private def sortMarketDataTypes(types:List[MarketDataType]) = types.sortWith(_.name.name < _.name.name)

  def latestMarketDataIdentifier(selection:MarketDataSelection):MarketDataIdentifier = marketDataStore.latestMarketDataIdentifier(selection)

}