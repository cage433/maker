package starling.rmi

import starling.daterange.Day
import starling.gui.api._
import starling.pivot.model.PivotTableModel
import starling.pivot.{NullPivotTableDataSource, PivotFieldParams, PivotEdits}
import starling.marketdata._
import starling.curves.{CurveViewer, EnvironmentRule}
import starling.fc2.api.FC2Facility
import starling.db._
import starling.utils.ImplicitConversions._
import starling.manager.BromptonContext


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
                      snapshotDatabase:MarketDataStore,
                      curveViewer : CurveViewer,
                      marketDataReaderProviders:MarketDataPageIdentifierReaderProviders,
                      referenceDataLookup: ReferenceDataLookup) extends FC2Facility {

  private def unLabel(pricingGroup:PricingGroup) = pricingGroup
  private def unLabel(snapshotID:SnapshotIDLabel) = snapshotDatabase.snapshotFromID(snapshotID.id).get
  private def label(pricingGroup:PricingGroup):PricingGroup = pricingGroup
  private def label(snapshot:SnapshotID):SnapshotIDLabel = snapshot.label

  def pricingGroups = {
    //val allPricingGroups = desks.flatMap(_.pricingGroups).toSet
    snapshotDatabase.pricingGroups//.filter(allPricingGroups.contains(_))
  }
  def excelDataSets() = snapshotDatabase.excelDataSets

  def environmentRules = {
    pricingGroups.map { pg => {
      val rules = pg match {
        case PricingGroup.Metals => EnvironmentRule.metalsRulesLabels
        case _ => EnvironmentRule.defaultRulesLabels
      }
      pg -> rules
    } }.toMap
  }

  val curveTypes = curveViewer.curveTypes

  def snapshots():Map[MarketDataSelection,List[SnapshotIDLabel]] = {
    snapshotDatabase.snapshotsByMarketDataSelection
  }

  def observationDays():(Map[PricingGroup,Set[Day]],Map[String,Set[Day]]) = {
    (Map() ++ snapshotDatabase.observationDaysByPricingGroup(), Map() ++ snapshotDatabase.observationDaysByExcel())
  }

  private def realTypeFor(label:MarketDataTypeLabel) = {
    MarketDataTypes.types.find(_.toString == label.name).getOrElse(throw new Exception("Can't find market data type '" + label.name + "' in " + MarketDataTypes.types))
  }

  def curvePivot(curveLabel: CurveLabel, pivotFieldParams: PivotFieldParams) = {
    PivotTableModel.createPivotData(curveViewer.curve(curveLabel), pivotFieldParams)
  }

  private def marketDataSource(marketDataIdentifier:MarketDataPageIdentifier, marketDataTypeLabel:Option[MarketDataTypeLabel], edits:PivotEdits) = {
    val reader = marketDataReaderFor(marketDataIdentifier)
    val marketDataType = marketDataTypeLabel match {
      case None => {
        sortMarketDataTypes(reader.marketDataTypes) match {
          case Nil => None
          case many => many.headOption
        }
      }
      case Some(mdt) => Some(realTypeFor(mdt))
    }
    marketDataType match {
      case Some(mdt) => new MarketDataPivotTableDataSource(reader, edits, Some(snapshotDatabase),
        marketDataIdentifier.marketDataIdentifier, mdt, referenceDataLookup)
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

  def snapshot(marketDataSelection:MarketDataSelection, observationDay:Day): Option[SnapshotIDLabel] = {
    snapshotDatabase.snapshot(marketDataSelection, true, observationDay).map(label)
  }

  def excelLatestMarketDataVersions = snapshotDatabase.latestExcelVersions
  def pricingGroupLatestMarketDataVersions = Map() ++ snapshotDatabase.latestPricingGroupVersions.filterKeys(pricingGroups()).toMap

  def latestSnapshotID(pricingGroup:PricingGroup, observationDay:Day) = {
    snapshotDatabase.latestSnapshot(pricingGroup, observationDay) match {
      case None => None
      case Some(x) => Some(label(x))
    }
  }

  private def marketDataReaderFor(marketDataIdentifier:MarketDataPageIdentifier) = {
    val reader = marketDataIdentifier match {
      case StandardMarketDataPageIdentifier(mdi) => new NormalMarketDataReader(snapshotDatabase, mdi)
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
    sortMarketDataTypes(marketDataReaderFor(marketDataIdentifier).marketDataTypes).map(t=>MarketDataTypeLabel(t.name))
  }

  private def sortMarketDataTypes(types:List[MarketDataType]) = types.sortWith(_.name < _.name)

  def latestMarketDataIdentifier(selection:MarketDataSelection):MarketDataIdentifier = snapshotDatabase.latestMarketDataIdentifier(selection)

}