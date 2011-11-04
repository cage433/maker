package starling.marketdata

import collection.Seq
import starling.curves.MarketDataSlice
import starling.pivot._
import model.UndefinedValue
import starling.daterange.{Day, ObservationPoint}
import starling.pivot.pivotparsers.DayPivotParser
import starling.richdb.RichDB
import starling.daterange.Timestamp
import starling.instrument.utils.StarlingXStream
import starling.dbx.{From, QueryBuilder, LiteralString}
import starling.daterange.{ObservationTimeOfDay, TimeOfDay, Day, ObservationPoint}
import starling.utils.ImplicitConversions._
import starling.gui.api._
import starling.db._
import starling.gui.api.{MarketDataIdentifier, MarketDataSelection, MarketDataVersion}
import starling.utils.cache.CacheFactory
import collection.immutable.{Map, Iterable}
import scalaz.Scalaz._
import java.lang.IllegalStateException

/**
 * Represents raw market data as pivot data
 *
 * Only shows one type of market data at a time as otherwise we have to show the union of all market data fields
 *
 * The implementation relies on methods on MarketDataType and MarketDataKey for the market data specific behaviour 
 */

object MarketDataPivotTableDataSource {
  val observationTimeField = FieldDetails("Observation Time")
  val observationDayField = FieldDetails("Observation Day", DayPivotParser)
}

class PrebuiltMarketDataPivotData(reader: MarketDataReader, marketDataStore: MarketDataStore,
  marketDataIdentifier: MarketDataIdentifier, val marketDataType:MarketDataType, dataTypes: MarketDataTypes) {

  import MarketDataPivotTableDataSource._
  val keyAndDataFields = marketDataType.fields.map(_.field).toSet

  val fieldDetailsGroups = List(
    FieldDetailsGroup("Market Data Fields", observationDayField :: observationTimeField :: marketDataType.fields)
  )
  lazy val fieldDetails:List[FieldDetails] = fieldDetailsGroups.flatMap(_.fields)

  val keyFields = marketDataType.keyFields + observationTimeField.field + observationDayField.field

  import QueryBuilder._

  import scala.collection.mutable.{HashSet=>MSet}

  val cache = CacheFactory.getCache("marketDataCache", unique=true)

  val observationDayAndMarketDataKeys = reader.readAllObservationDayAndMarketDataKeys(marketDataType.name)

  val observationDayAndMarketDataKeyRows: Map[Row, TimedMarketDataKey] = observationDayAndMarketDataKeys.map { timedKey =>
    (timedKey.fieldValues(marketDataType) + (observationTimeField.field → timedKey.timeName)
      +? (observationDayField.field → timedKey.day)) → timedKey
  }.toMap
  val allMarketDataKeys = observationDayAndMarketDataKeys.map(_.key).toSet

  val inMemoryFields = observationDayAndMarketDataKeyRows.keys.flatMap(_.fields).toSet

  val initialState = {
    val observationDaySelection = if (observationDayAndMarketDataKeyRows.isEmpty) {
      new SomeSelection(Set())
    } else {
      val days = observationDayAndMarketDataKeyRows.map( rt => rt._1.get(observationDayField.field).asInstanceOf[Option[Day]]).toList
      val selection = if (days.contains(None)) UndefinedValue else days.somes.max
      new SomeSelection(Set(selection))
    }

    val marketDataTypeInitialPivotState = marketDataType.initialPivotState
    marketDataTypeInitialPivotState.copy(filters=(observationDayField.field, observationDaySelection) :: marketDataTypeInitialPivotState.filters)
  }

  private lazy val editableMarketDataSet = marketDataIdentifier.selection match {
    case MarketDataSelection(None, None) => None
    case MarketDataSelection(_, Some(excelName)) => Some(MarketDataSet.excel(excelName))
    case MarketDataSelection(Some(pricingGroup), _) => MarketDataStore.editableMarketDataSetFor(pricingGroup)
  }

  private def buildModifiedRows(currentRows: scala.Iterable[Row], maybeEdits: Option[scala.List[(KeyFilter, KeyEdits)]]): List[Row] = {
    var modifiedRows = currentRows

    maybeEdits match {
      case Some(edits0) => {
        edits0.foreach {
          case (keyFilter, keyEdit) => {
            val (affectedRows, ignoredRows) = modifiedRows.partition(keyFilter.matches)
            val fixedRows = keyEdit match {
              case DeleteKeyEdit => Nil
              case AmendKeyEdit(amends) => {
                if (amends.values.toSet.contains(None)) {
                  Nil //Delete the row if the measure has been deleted
                } else {
                  affectedRows.map {
                    row => row ++ amends.filter(_._2.isDefined).mapValues(_.get)
                  }
                }
              }
            }
            modifiedRows = (fixedRows.toList ::: ignoredRows.toList)
            // modifiedRows = (fixedRows.toList ::: newRows ::: ignoredRows.toList) // doesn't this add duplicates of newRows ?
          }
        }
      }
      case None => {}
    }

    modifiedRows.toList
  }

  def checkForDuplicates(key:TimedMarketDataKey, rows:List[Row], message:String) {
    val keys: List[FieldDetails] = marketDataType.valueKeys.toList
    val rowsForKey = rows.groupBy(r => keys.map(r[Any](_)))
    rowsForKey.foreach { case (k, rows) => {
      if (rows.size > 1) {
        throw new IllegalStateException(message + " " + key.fieldValues(marketDataType) + " " + k)
      }
    }}
  }

  def buildUpdateForNewKey(key: TimedMarketDataKey, originalKey: TimedMarketDataKey, currentMarketData: VersionedMarketData, rows: scala.List[Row], editableSet: MarketDataSet): MarketDataUpdate = {
    val (existingData, newMarketData) = if (key == originalKey) {
      checkForDuplicates(key, rows, "You can't create a new row because there is already an existing row for ")
      (Some(currentMarketData), marketDataType.createValue(rows)) //adding new rows or amending values for originalKey
    } else {
      marketDataStore.readLatest(editableSet, key) match {
        case vmd@Some(VersionedMarketData(_, Some(existingMarketData))) => { //edited an 'originalKey' value so that it is stored under 'key'
          val joinedRows = rows ::: dataType(key.key).castRows(key.key, existingMarketData).toList
          checkForDuplicates(key, joinedRows, "You can't make this edit because it will override existing values for ")
          (vmd, marketDataType.createValue(joinedRows))
        }
        case _ => { //edited a value which makes it create a new TimedMarketDataKey
          checkForDuplicates(key, rows, "The new rows define more than one value for ")
          (None, marketDataType.createValue(rows))
        }
      }
    }

    MarketDataEntry(key.observationPoint, key.key, newMarketData).toUpdate(existingData)
  }

  private def dataType(key: MarketDataKey): MarketDataType = dataTypes.fromName(key.typeName)

  def createRows(timedKey:TimedMarketDataKey, marketData:MarketData) ={
    dataType(timedKey.key).castRows(timedKey.key, marketData).map { _ + (observationTimeField.field → timedKey.timeName) +?
     (observationDayField.field → timedKey.day)}
  }


  def buildUpdates(timedKey: TimedMarketDataKey, editableSet: MarketDataSet,
                   maybeEdits: Option[scala.List[(KeyFilter, KeyEdits)]], newRows: scala.List[Row]): List[MarketDataUpdate] = {

    val latest: Option[VersionedMarketData] = marketDataStore.readLatest(editableSet, timedKey)

    latest match {
      case Some(v@VersionedMarketData(_, Some(readData))) => {
        val currentRows = createRows(timedKey, readData)
        val modifiedCurrentRows = buildModifiedRows(currentRows, maybeEdits)

        val keysForNewData: Map[TimedMarketDataKey, List[Row]] = (modifiedCurrentRows ::: newRows).groupBy { row => {
          val observationPoint = ObservationPoint(row[Day](observationDayField.field),
            ObservationTimeOfDay.fromName(row.string(observationTimeField)))

          TimedMarketDataKey(observationPoint, marketDataType.createKey(row))
        } }

        val updates = keysForNewData.map { case (key, rows) => buildUpdateForNewKey(key, timedKey, v, rows, editableSet) }.toList

        keysForNewData.contains(timedKey) ? updates | (MarketDataUpdate(timedKey, None, Some(v)) :: updates)
      }
      case _ => {
        val modifiedRows = maybeEdits.map(_.flatMap {
          case (keyFilter, keyEdit) => {
            keyEdit match {
              case DeleteKeyEdit => Nil //Ignore, we can't override existing values with a 'delete'
              case AmendKeyEdit(amends) => {
                if (amends.values.toSet.contains(None)) {
                  Nil //Ignore, we can't override existing values with a 'delete'
                } else {
                  Row(keyFilter.keys.mapValues(_.values.iterator.next) ++ marketDataType.defaultValue.value ++ amends.mapValues(_.get)) :: Nil
                }
              }
            }
          }
        }).getOrElse(Nil)
        List(MarketDataEntry(timedKey.observationPoint, timedKey.key, marketDataType.createValue(modifiedRows.toList ::: newRows)).toUpdate(None))
      }
    }
  }

  def editable = editableMarketDataSet.map { editableSet =>
    new EditPivot {
      private val keyFields = Set(observationDayField.field, observationTimeField.field) +++ marketDataType.keyFields
      private val keyAndValueFields = (keyFields +++ marketDataType.valueFields.toSet)
      def editableToKeyFields = Map() ++ marketDataType.valueFields.map((_ -> keyFields))
      def withEdits(edits:PivotEdits) = new MarketDataPivotTableDataSource(PrebuiltMarketDataPivotData.this, edits)

      def save(edits:PivotEdits) = {
        val editsByTimedKeys: Map[TimedMarketDataKey, List[(KeyFilter, KeyEdits)]] = edits.edits.toList.flatMap { case(keyFilter, keyEdit) => {
          val keyFilterForTimedKey = keyFilter.retain(inMemoryFields)
          val matchingTimedKeys = observationDayAndMarketDataKeyRows.filterKeys(keyFilterForTimedKey.matches).values

          matchingTimedKeys.pair((keyFilter.remove(inMemoryFields), keyEdit))
        } }.groupBy(_._1).mapValues(_.map(_._2))

        val newRowsWithAllFieldsPresent = edits.newRows.map(marketDataType.defaultValue + _).filter { row =>
          keyAndValueFields.forall{ field => row.isDefined(field) }
        }

        val groupedNewRows: Map[TimedMarketDataKey, List[Row]] = newRowsWithAllFieldsPresent.groupBy { row => {
          val observationPoint = ObservationPoint(
            row(observationDayField.field).asInstanceOf[Day],
            ObservationTimeOfDay.fromName(row.string(observationTimeField))
          )
          val key: MarketDataKey = marketDataType.createKey(row)
          TimedMarketDataKey(observationPoint, key)
        }}

        val newEntries = (groupedNewRows.keySet +++ editsByTimedKeys.keySet).flatMap { timedKey =>
          buildUpdates(timedKey, editableSet, editsByTimedKeys.get(timedKey), groupedNewRows.getOrElse(timedKey, Nil))
        }

        val r:SaveResult = marketDataStore.update(Map(editableSet → newEntries.toList))
        r.maxVersion
      }
    }
  }

  def dataWithoutEdits(pfs : PivotFieldsState): (Map[Field, List[Any]], List[Row]) =
    cache.memoize( (pfs), {
      generateDataWithoutEdits(pfs)
    } )

  private def generateDataWithoutEdits(pfs : PivotFieldsState): (Map[Field, List[Any]], List[Row]) = {

    val filtersUpToFirstMarketDataField = pfs.allFilterPaths.chopUpToFirstNon(inMemoryFields)
    //FIXME toSet.toList probably means the filtering is incorrect when there is more than one filter path
    val foo = filtersUpToFirstMarketDataField.flatten.toSet
    val filters: Set[(Field, Selection)] = foo.flatMap {
      case (field,MeasurePossibleValuesFilter(_)) => Nil
      case (field,SelectionPossibleValuesFilter(selection)) => List(field -> selection)
    }

    val possibleValuesBuilder = new PossibleValuesBuilder(fieldDetails, filtersUpToFirstMarketDataField)
    for ((row,_) <- observationDayAndMarketDataKeyRows) {
      possibleValuesBuilder += row.value
    }

    val marketDataKeyFields = allMarketDataKeys.headOption.fold(_.fields.toList, Nil)

    val allFields = pfs.allFieldsUsed

    val marketDataFields = allFields.toList filterNot( f => (observationDayField.field :: observationTimeField.field :: marketDataKeyFields).contains(f) )

    val data = if (marketDataFields.isEmpty) observationDayAndMarketDataKeyRows.keys.toList else {

      def selectedValues[T : Manifest](f: Field): Option[Set[Option[T]]] = {
        filters.find { case (field, selection) => field == f && selection.isInstanceOf[SomeSelection] }
          .map { case (_, SomeSelection(values)) => {
          val valuesOfType = values.toList.filterCast[T]
          val naValue = if (values.contains(UndefinedValue)) List(None) else List()
          (naValue ::: valuesOfType.map(Some(_))).toSet
        } }
      }

      val keyFilters: Set[(Field, Set[Any])] = filters.flatMap{ case(field,selection) => if(keyAndDataFields.contains(field)) selection match { case AllSelection => None; case SomeSelection(values) => Some( field->values)} else None }

      val keyClause: Option[Set[MarketDataKey]] = if(keyFilters.isEmpty) None else {
        val filterKeys: Set[MarketDataKey] = allMarketDataKeys.filter { key => {
          val fieldValuesForkey: Row = marketDataType.fieldValues(key)
          keyFilters.forall { case (field, values) =>
            fieldValuesForkey.get[Any](field).map(value => values.contains(value)).getOrElse(true)
          }
        } }

        Some(filterKeys)
      }

      val observationDays = selectedValues[Day](observationDayField.field)
      val observationTimes: Option[Set[ObservationTimeOfDay]] = selectedValues[String](observationTimeField.field).map(_.map(t=>ObservationTimeOfDay.fromName(t.get)))

      val allData = reader.read(marketDataType.name, observationDays, observationTimes, keyClause)
      allData.flatMap { case (timedKey, data) => {
        dataType(timedKey.key).castRows(timedKey.key, data).map { row => {
          row + (observationTimeField.field → timedKey.timeName) +? (observationDayField.field → timedKey.day)
        } }
      } }
    }
    (possibleValuesBuilder.build, data)
  }
}

class MarketDataPivotTableDataSource(preBuilt:PrebuiltMarketDataPivotData, edits:PivotEdits) extends PivotTableDataSource {

  val marketDataType = preBuilt.marketDataType
  val fieldDetailsGroups = preBuilt.fieldDetailsGroups

  override def zeroFields = marketDataType.zeroFields.toSet

  override def initialState = preBuilt.initialState
  override def editable = preBuilt.editable

  def data(pfs : PivotFieldsState):PivotResult = {

    val (initialPossibleValues, data) = preBuilt.dataWithoutEdits(pfs)

    val filtersUpToFirstMarketDataField = pfs.allFilterPaths.chopUpToFirstNon(preBuilt.inMemoryFields)
    val possibleValuesBuilder = new PossibleValuesBuilder(fieldDetails, filtersUpToFirstMarketDataField)

    possibleValuesBuilder.init(initialPossibleValues)

    val editedData = edits.applyTo(data)
    val allFields = pfs.allFieldsUsed

    val addedRows = edits.newRows.zipWithIndex.map{case (row,index) => {
      val fixedRow = if (marketDataType.marketDataKeyFields.forall(f => row.isDefined(f))) {
        row + marketDataType.fieldValues(row)
      } else {
        row
      }
      Map() ++ allFields.map(f => {
        f -> NewValue(fixedRow.get(f), index, PivotEdits.Null.withAddedRow(fixedRow))
      })
    }}.toList

    for (row <- addedRows) {
      possibleValuesBuilder += row
    }

    val allRows = editedData.map(_.value) ::: addedRows

    val result = UnfilteredPivotTableDataSource.applyFiltersAndCalculatePossibleValues(fieldDetails, allRows, pfs)//.removeAll(filtersUpToFirstMarketDataField.allFields))

    PivotResult(result.data, result.possibleValues ++ possibleValuesBuilder.build)
  }

}