package starling.marketdata

import collection.Seq
import starling.curves.MarketDataSlice
import starling.pivot._
import model.UndefinedValue
import starling.daterange.{Day, ObservationPoint}
import starling.pivot.pivotparsers.DayPivotParser
import starling.richdb.RichDB
import starling.daterange.Timestamp
import starling.utils.StarlingXStream
import starling.utils.sql.{From, QueryBuilder, LiteralString}
import starling.daterange.{ObservationTimeOfDay, TimeOfDay, Day, ObservationPoint}
import starling.utils.ImplicitConversions._
import starling.gui.api._
import starling.db._
import starling.gui.api.{MarketDataIdentifier, MarketDataSelection, MarketDataVersion}

/**
 * Represents raw market data as pivot data
 *
 * Only shows one type of market data at a time as otherwise we have to show the union of all market data fields
 *
 * The implementation relies on methods on MarketDataType and MarketDataKey for the market data specific behaviour 
 */

class MarketDataPivotTableDataSource(reader: MarketDataReader, marketDataStore:Option[MarketDataStore],marketDataIdentifier: MarketDataIdentifier, val marketDataType:MarketDataType)
  extends PivotTableDataSource {

  val observationTimeField = FieldDetails("Observation Time")
  val observationDayField = FieldDetails("Observation Day", DayPivotParser)
  val keyAndDataFields = marketDataType.fields.map(_.field).toSet
  val fieldDetailsGroups = List(
    FieldDetailsGroup("Market Data Fields", observationDayField :: observationTimeField :: marketDataType.fields)
  )
  private val fieldToFieldDetails : Map[Field, FieldDetails]= Map() ++ fieldDetails.map(f=>f.field->f)

  import QueryBuilder._

  import scala.collection.mutable.{HashSet=>MSet}

  val observationDayAndMarketDataKeys = reader.readAllObservationDayAndMarketDataKeys(marketDataType)

  val observationDayAndMarketDataKeyRows: List[Map[Field, Any]] = observationDayAndMarketDataKeys.map { timedKey =>
    timedKey.fieldValues + (observationTimeField.field → timedKey.timeName) addSome (observationDayField.field → timedKey.day)
  }
  val allMarketDataKeys = observationDayAndMarketDataKeys.map(_.key).toSet

  val inMemoryFields = observationDayAndMarketDataKeyRows.flatMap(_.keySet).toSet

  override val initialState = {
    val observationDaySelection = if (observationDayAndMarketDataKeyRows.isEmpty) {
      new SomeSelection(Set())
    } else {
      val days = observationDayAndMarketDataKeyRows.map( r => r.get(observationDayField.field).asInstanceOf[Option[Day]])
      val selection = if (days.contains(None)) UndefinedValue else days.somes.max
      new SomeSelection(Set(selection))
    }

    val marketDataTypeInitialPivotState = marketDataType.initialPivotState
    marketDataTypeInitialPivotState.copy(filters=(observationDayField.field, observationDaySelection) :: marketDataTypeInitialPivotState.filters)
  }

  def editableMarketDataSet = {
    marketDataIdentifier.selection match {
      case MarketDataSelection(None, None) => None
      case MarketDataSelection(_, Some(excelName)) => Some(MarketDataSet.excel(excelName))
      case MarketDataSelection(Some(pricingGroup), _) => MarketDataStore.editableMarketDataSetFor(pricingGroup)
    }
  }

  override def editable = {
    marketDataStore.flatMap { marketDataStore =>
     editableMarketDataSet.map { editableSet =>
      new EditPivot {
        private def keyFields = Set(observationDayField.field, observationTimeField.field) ++ marketDataType.keyFields
        def editableToKeyFields = Map() ++ marketDataType.valueFields.map((_ -> keyFields))
        def save(ungroupedEdits:Set[PivotEdit]) = {

          val groupedEdits: Map[(ObservationPoint, MarketDataKey), List[PivotEdit]] = ungroupedEdits.groupBy(edit => {
            val observationPoint = ObservationPoint(
              edit.value[Day](observationDayField.field),
              ObservationTimeOfDay.fromName(edit.value[String](observationTimeField.field))
            )

            val key: MarketDataKey = marketDataType.createKey(edit.values)
            (observationPoint, key)
          }).mapValues(_.toList)

          val data = groupedEdits.map{case ((observationPoint, key), edits) => {
            val latest: Option[MarketData] = marketDataStore.readLatest(editableSet, TimedMarketDataKey(observationPoint, key))
            val editedData = latest match {
              case Some(readData) => {
                val currentRows = key.castRows(readData)
                // Remove deletes from current rows.
                val deletes: List[DeletePivotEdit] = edits.filterCast[DeletePivotEdit]
                val currentRowsWithDeletesRemoved = currentRows.filterNot(currentMap => {
                  deletes.exists(del => del.matches(currentMap, observationDayField.field, observationTimeField.field))
                })

                // Add amendments
                val amendments = edits.filterCast[AmendPivotEdit]
                val newRows = amendments.toList.map(_.values)
                val mixedRows = marketDataStore.applyOverrideRule(marketDataType, currentRowsWithDeletesRemoved.toList ::: newRows.toList).toList

                marketDataType.createValue(mixedRows)
              }
              case None => {
                // There should be no deletes here if the readData isn't defined.
                marketDataType.createValue(edits.toList.map(_.values))
              }
            }

            MarketDataEntry(observationPoint, key, editedData)
          } }
          marketDataStore.save(Map(editableSet -> data))._2
        }
      }
     }
    }
  }

  def data(pfs : PivotFieldsState):PivotResult = {

//    if (pfs.filterAreaFields.isEmpty || !inMemoryFields.contains(pfs.filterAreaFields.head)) {
//      //If there is no filter area filter or if the filter is on something like period
//      //then all the market data for all observation days will be read which will probably take ages
//      //so for now we just return nothing
//      return PivotResult(List(), Map())
//    }

    val filtersUpToFirstMarketDataField = pfs.allFilterPaths.chopUpToFirstNon(inMemoryFields)
    val filters: Set[(Field, Selection)] = filtersUpToFirstMarketDataField.toFilterSet

    val possibleValuesBuilder = new PossibleValuesBuilder(fieldDetails, filtersUpToFirstMarketDataField)
    for (row <- observationDayAndMarketDataKeyRows) {
      possibleValuesBuilder += row
    }

    val marketDataKeyFields = allMarketDataKeys.headOption match {
      case Some(mdf) => mdf.fieldValues.keySet.toList
      case None => List()
    }

    val allFields = pfs.allFieldsUsed

    val marketDataFields = allFields.toList filterNot( f => (observationDayField.field :: observationTimeField.field :: marketDataKeyFields).contains(f) )

    val data = if (marketDataFields.isEmpty) {
      observationDayAndMarketDataKeyRows
    } else {
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
          val fieldValuesForkey: Map[starling.pivot.Field,Any] = key.fieldValues
          keyFilters.forall { case (field, values) =>
            fieldValuesForkey.get(field).map(value => values.contains(value)).getOrElse(true)
          }
        } }

        Some(filterKeys)
      }

      val observationDays = selectedValues[Day](observationDayField.field)
      val observationTimes = selectedValues[String](observationTimeField.field).map(_.map(t=>ObservationTimeOfDay.fromName(t.get)))

      val allData = reader.read(marketDataType, observationDays, observationTimes, keyClause)

      allData.flatMap { case (timedKey, data) => {
        timedKey.castRows(data).map(_ + (observationTimeField.field → timedKey.timeName) addSome (observationDayField.field → timedKey.day))
      } }
    }
    val result = UnfilteredPivotTableDataSource.applyFiltersAndCalculatePossibleValues(fieldDetails, data, pfs)//.removeAll(filtersUpToFirstMarketDataField.allFields))
    PivotResult(result.data, result.possibleValues ++ possibleValuesBuilder.build)
  }
}
