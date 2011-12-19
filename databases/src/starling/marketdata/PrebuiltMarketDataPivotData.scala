package starling.marketdata

import starling.pivot._
import model.UndefinedValue
import starling.daterange.{ObservationTimeOfDay, Day, ObservationPoint}
import starling.utils.ImplicitConversions._
import starling.db._
import starling.gui.api.{MarketDataIdentifier, MarketDataSelection}
import starling.utils.cache.CacheFactory
import collection.immutable.Map
import scalaz.Scalaz._
import java.lang.IllegalStateException


class PrebuiltMarketDataPivotData(reader: MarketDataReader, marketDataStore: MarketDataStore,
  marketDataIdentifier: MarketDataIdentifier, val marketDataType:MarketDataType, dataTypes: MarketDataTypes) {

  val cache = CacheFactory.getCache("marketDataCache", unique=true)

  import MarketDataPivotTableDataSource._

  private implicit def enrichRow(row: Row) = new {
    def appendObservationDayAndTime(timedKey: TimedMarketDataKey): Row =
      row + (observationTimeField.field → timedKey.timeName) +? (observationDayField.field → timedKey.day)
  }

  val keyAndDataFields = marketDataType.fields.map(_.field).toSet

  val fieldDetailsGroups = List(
    FieldDetailsGroup("Market Data Fields", observationDayField :: observationTimeField :: marketDataType.fields)
  )
  lazy val fieldDetails:List[FieldDetails] = fieldDetailsGroups.flatMap(_.fields)

  val keyFields = marketDataType.keyFields + observationTimeField.field + observationDayField.field

  val observationDayAndMarketDataKeys = reader.readAllObservationDayAndMarketDataKeys(marketDataType.name)
  val allMarketDataKeys = observationDayAndMarketDataKeys.map(_.key).toSet

  val observationDayAndMarketDataKeyRows: Map[Row, TimedMarketDataKey] =
    observationDayAndMarketDataKeys.toMapWithKeys { timedKey => (timedKey.fieldValues(marketDataType).appendObservationDayAndTime(timedKey)) }

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
    val pfs = if (marketDataTypeInitialPivotState.fieldSelection(observationDayField.field).isDefined) {
      marketDataTypeInitialPivotState
    } else {
      marketDataTypeInitialPivotState.copy(filters=(observationDayField.field, observationDaySelection) :: marketDataTypeInitialPivotState.filters)
    }
    DefaultPivotState(pfs, OtherLayoutInfo(frozen = false))
  }

  private lazy val editableMarketDataSet = marketDataIdentifier.selection match {
    case MarketDataSelection(None, None) => None
    case MarketDataSelection(_, Some(excelName)) => Some(MarketDataSet.excel(excelName))
    case MarketDataSelection(Some(pricingGroup), _) => MarketDataStore.editableMarketDataSetFor(pricingGroup)
  }

  private def buildModifiedRows(currentRows: scala.Iterable[Row], maybeEdits: Option[scala.List[(KeyFilter, KeyEdits)]]): List[Row] = {
    var modifiedRows = currentRows

    maybeEdits.foreach { _.foreach { case (keyFilter, keyEdit) => {
      val (affectedRows, ignoredRows) = modifiedRows.partition(keyFilter.matches)
      val fixedRows = keyEdit match {
        case DeleteKeyEdit => Nil
        case AmendKeyEdit(amends) => {
          if (amends.values.toSet.contains(None)) Nil else { //Delete the row if the measure has been deleted
            affectedRows.map { _ ++ amends.flatMapValues(identity) }
          }
        }
      }

      modifiedRows = (fixedRows.toList ::: ignoredRows.toList)
      // modifiedRows = (fixedRows.toList ::: newRows ::: ignoredRows.toList) // doesn't this add duplicates of newRows ?
    } } }

    modifiedRows.toList
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

  private def checkForDuplicates(key:TimedMarketDataKey, rows:List[Row], message:String) {
    val keys: List[FieldDetails] = marketDataType.valueKeys.toList
    val rowsForKey = rows.groupBy(r => keys.map(r[Any](_)))
    rowsForKey.foreach { case (k, rows) => {
      if (rows.size > 1) {
        throw new IllegalStateException(message + " " + key.fieldValues(marketDataType) + " " + k)
      }
    }}
  }

  private def dataType(key: MarketDataKey): MarketDataType = dataTypes.fromName(key.typeName)

  def buildUpdates(timedKey: TimedMarketDataKey, editableSet: MarketDataSet,
                   maybeEdits: Option[scala.List[(KeyFilter, KeyEdits)]], newRows: scala.List[Row]): List[MarketDataUpdate] = {

    val latest: Option[VersionedMarketData] = marketDataStore.readLatest(editableSet, timedKey)

    latest match {
      case Some(v@VersionedMarketData(_, Some(readData))) => {
        val currentRows = createRows(timedKey, readData)
        val modifiedCurrentRows = buildModifiedRows(currentRows, maybeEdits)

        val keysForNewData: Map[TimedMarketDataKey, List[Row]] = (modifiedCurrentRows ::: newRows).groupBy { row => {
          val observationPoint = row.get[Any](observationDayField.field) match {
            case None | Some(UndefinedValue) => ObservationPoint.RealTime
            case Some(day:Day) => {
              ObservationPoint(day, ObservationTimeOfDay.fromName(row.string(observationTimeField)))
            }
          }
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
      val keyFields = Set(observationDayField.field, observationTimeField.field) +++ marketDataType.keyFields
      private val keyAndValueFields = (keyFields +++ marketDataType.valueFields.toSet)
      def editableToKeyFields = Map() ++ marketDataType.valueFields.map((_ -> keyFields))
      def withEdits(edits:PivotEdits) = new MarketDataPivotTableDataSource(PrebuiltMarketDataPivotData.this, edits)

      def save(edits:PivotEdits) = {

        println("EDITS " + edits)

        val editsByTimedKeys: Map[TimedMarketDataKey, List[(KeyFilter, KeyEdits)]] = edits.edits.toList.flatMap { case(keyFilter, keyEdit) => {
          val keyFilterForTimedKey = keyFilter.retain(inMemoryFields)
          val matchingTimedKeys = observationDayAndMarketDataKeyRows.filterKeys(keyFilterForTimedKey.matches).values

          matchingTimedKeys.pairWith((keyFilter.remove(inMemoryFields), keyEdit))
        } }.groupBy(_._1).mapValues(_.map(_._2))

        val newRowsWithAllFieldsPresent = edits.newRows.map(marketDataType.defaultValue + _).filter { row =>
          keyAndValueFields.forall{ field => row.isDefined(field) || field == observationDayField.field }
        }

        val groupedNewRows: Map[TimedMarketDataKey, List[Row]] = newRowsWithAllFieldsPresent.groupBy { row => {
          val observationPoint = {
            if (row(observationDayField.field) == UndefinedValue) {
              ObservationPoint.RealTime
            } else {
              ObservationPoint(
                row(observationDayField.field).asInstanceOf[Day],
                ObservationTimeOfDay.fromName(row.string(observationTimeField))
              )
            }
          }
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
    val foo: Set[(Field, PossibleValuesFilter)] = filtersUpToFirstMarketDataField.flatten.toSet
    val filters: Map[Field, Selection] = foo.flatMap {
      case (field,MeasurePossibleValuesFilter(_)) => Nil
      case (field,SelectionPossibleValuesFilter(selection)) => List(field → selection)
    }.toMap

    val possibleValuesBuilder = new PossibleValuesBuilder(fieldDetails, filtersUpToFirstMarketDataField)
    observationDayAndMarketDataKeyRows.foreach { case (row, _) => possibleValuesBuilder += row.value }

    def possibleSelectedValues[T: Manifest](field: Field): Option[Set[Option[T]]] = filters.get(field) partialMatch {
      case Some(s: SomeSelection) => possibleValuesBuilder.selectedValues[T](field, s).toSet
    }

    val marketDataFields = {
      val toExclude = observationDayField.field :: observationTimeField.field :: allMarketDataKeys.headOption.fold(_.fields.toList, Nil)

      pfs.allFieldsUsed filterNot(toExclude)
    }

    val data = if (marketDataFields.isEmpty) observationDayAndMarketDataKeyRows.keys.toList else {
      val keyFilters: Map[Field, Set[Any]] =
        filters.filterKeys(keyAndDataFields).flatMapValues { _ partialMatch { case SomeSelection(values) => values } }

      val keyClause: Option[Set[MarketDataKey]] = keyFilters.ifDefined { _ =>
        allMarketDataKeys.filter { marketDataType.fieldValues(_).matchesAll(keyFilters) }
      }

      val observationDays = possibleSelectedValues[Day](observationDayField.field)
      val observationTimes = possibleSelectedValues[String](observationTimeField.field).map(_.flatMap(ObservationTimeOfDay.fromName(_)))

      val allData = reader.read(marketDataType.name, observationDays, observationTimes, keyClause)

      allData.flatMap((createRows _).tupled)
    }

    (possibleValuesBuilder.build, data)
  }

  private def createRows(timedKey: TimedMarketDataKey, marketData: MarketData) = {
    dataType(timedKey.key).castRows(timedKey.key, marketData).map(_.appendObservationDayAndTime(timedKey))
  }
}