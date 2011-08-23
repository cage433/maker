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
import starling.utils.cache.CacheFactory
import collection.immutable.{Map, Iterable}

/**
 * Represents raw market data as pivot data
 *
 * Only shows one type of market data at a time as otherwise we have to show the union of all market data fields
 *
 * The implementation relies on methods on MarketDataType and MarketDataKey for the market data specific behaviour 
 */

class MarketDataPivotTableDataSource(reader: MarketDataReader, edits:PivotEdits, marketDataStore:Option[MarketDataStore],marketDataIdentifier: MarketDataIdentifier, val marketDataType:MarketDataType)
  extends PivotTableDataSource {

  val observationTimeField = FieldDetails("Observation Time")
  val observationDayField = FieldDetails("Observation Day", DayPivotParser)
  val keyAndDataFields = marketDataType.fields.map(_.field).toSet
  val fieldDetailsGroups = List(
    FieldDetailsGroup("Market Data Fields", observationDayField :: observationTimeField :: marketDataType.fields)
  )

  val keyFields = marketDataType.keyFields + observationTimeField.field + observationDayField.field
  private val fieldToFieldDetails : Map[Field, FieldDetails]= Map() ++ fieldDetails.map(f=>f.field->f)

  import QueryBuilder._

  import scala.collection.mutable.{HashSet=>MSet}

  //val cache = CacheFactory.getCache("cache")

  val observationDayAndMarketDataKeys = reader.readAllObservationDayAndMarketDataKeys(marketDataType)

  val observationDayAndMarketDataKeyRows: Map[Map[Field, Any],TimedMarketDataKey] = observationDayAndMarketDataKeys.map { timedKey =>
    (timedKey.fieldValues + (observationTimeField.field → timedKey.timeName) addSome (observationDayField.field → timedKey.day)) -> timedKey
  }.toMap
  val allMarketDataKeys = observationDayAndMarketDataKeys.map(_.key).toSet

  val inMemoryFields = observationDayAndMarketDataKeyRows.keys.flatMap(_.keySet).toSet

  override val initialState = {
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
        private val keyFields = Set(observationDayField.field, observationTimeField.field) ++ marketDataType.keyFields
        private val keyAndValueFields = (keyFields ++ marketDataType.valueFields)
        def editableToKeyFields = Map() ++ marketDataType.valueFields.map((_ -> keyFields))
        def withEdits(edits:PivotEdits):PivotTableDataSource = {
          new MarketDataPivotTableDataSource(reader, edits, Some(marketDataStore), marketDataIdentifier, marketDataType)
        }
        def save(edits:PivotEdits) = {

          val editsByTimedKeys: Map[TimedMarketDataKey, List[(KeyFilter, KeyEdits)]] = edits.edits.toList.flatMap { case(keyFilter, keyEdit) => {
            val keyFilterForTimedKey = keyFilter.retain(inMemoryFields)
            val matchingTimedKeys = observationDayAndMarketDataKeyRows.toList.filter { case (row,timedKey) => {
              keyFilterForTimedKey.matches(row)
            } }
            matchingTimedKeys.map {
              case (row,timedKey) => {
                val fixedKeyFilter = keyFilter.remove(inMemoryFields)
                (timedKey, (fixedKeyFilter, keyEdit))
              }
            } }
          }.groupBy(_._1).mapValues(_.map(_._2))

          val newRowsWithAllFieldsPresent = edits.newRows.filter { row => {
            keyAndValueFields.forall{ field => {
              row.get(field) match {
                case None => false
                case Some(UndefinedValue) => false
                case Some(_) => true
              }
            } }
          }}

          val groupedNewRows: Map[TimedMarketDataKey, List[Map[Field, Any]]] = newRowsWithAllFieldsPresent.groupBy { row => {
            val observationPoint = ObservationPoint(
              row(observationDayField.field).asInstanceOf[Day],
              ObservationTimeOfDay.fromName(row(observationTimeField.field).asInstanceOf[String])
            )
            val key: MarketDataKey = marketDataType.createKey(row)
            TimedMarketDataKey(observationPoint, key)
          }}

          val newEntries = (groupedNewRows.keySet ++ editsByTimedKeys.keySet).map { timedKey => {
            val newRows = groupedNewRows.getOrElse(timedKey, Nil)
            val maybeEdits = editsByTimedKeys.get(timedKey)

            val latest: Option[MarketData] = marketDataStore.readLatest(editableSet, timedKey)
            val amendRows = latest match {
              case None => {
                maybeEdits.map( _.flatMap { case (keyFilter, keyEdit) => {
                  keyEdit match {
                    case DeleteKeyEdit => Nil //Ignore, we can't override existing values with a 'delete'
                    case AmendKeyEdit(amends) => {
                      if (amends.values.toSet.contains(None)) {
                        Nil //Ignore, we can't override existing values with a 'delete'
                      } else {
                        keyFilter.keys.mapValues(_.values.iterator.next) ++ amends.mapValues(_.get) ::Nil
                      }
                    }
                  }
                }}).getOrElse(Nil)
              }
              case Some(readData) => {
                val currentRows = timedKey.key.castRows(readData)
                var modifiedRows = currentRows

                maybeEdits match {
                  case Some(edits0) => {
                    edits0.foreach { case (keyFilter, keyEdit) => {
                      modifiedRows = modifiedRows.flatMap { row => {
                        if (keyFilter.matches(row)) {
                            keyEdit match {
                              case DeleteKeyEdit => None
                              case AmendKeyEdit(amends) => {
                                if (amends.values.toSet.contains(None)) {
                                  None //Delete the row if the measure has been deleted
                                } else {
                                  Some( row ++ amends.filter(_._2.isDefined).mapValues(_.get) )
                                }
                              }
                            }
                          } else {
                            Some(row)
                          }
                        } }
                      } }
                  }
                  case None => {}
                }
                modifiedRows.toList
              }
            }
            MarketDataEntry(timedKey.observationPoint, timedKey.key, marketDataType.createValue(amendRows ::: newRows))
          } }

          marketDataStore.save(Map(editableSet → newEntries))
          true
        }
      }
     }
    }
  }

  private def dataWithoutEdits(pfs : PivotFieldsState) = {
    generateDataWithoutEdits(pfs)
    //cache.memoize(pfs, { generateDataWithoutEdits(pfs) })
  }

  private def generateDataWithoutEdits(pfs : PivotFieldsState) = {

    val filtersUpToFirstMarketDataField = pfs.allFilterPaths.chopUpToFirstNon(inMemoryFields)
    val filters: Set[(Field, Selection)] = filtersUpToFirstMarketDataField.toFilterSet

    val possibleValuesBuilder = new PossibleValuesBuilder(fieldDetails, filtersUpToFirstMarketDataField)
    for ((row,_) <- observationDayAndMarketDataKeyRows) {
      possibleValuesBuilder += row
    }

    val marketDataKeyFields = allMarketDataKeys.headOption match {
      case Some(mdf) => mdf.fieldValues.keySet.toList
      case None => List()
    }

    val allFields = pfs.allFieldsUsed

    val marketDataFields = allFields.toList filterNot( f => (observationDayField.field :: observationTimeField.field :: marketDataKeyFields).contains(f) )

    val data = if (marketDataFields.isEmpty) {
      observationDayAndMarketDataKeyRows.keys
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
        timedKey.castRows(data).map { row => {
          row + (observationTimeField.field → timedKey.timeName) addSome (observationDayField.field → timedKey.day)
        } }
      } }
    }
    (possibleValuesBuilder.build, data)
  }

  def data(pfs : PivotFieldsState):PivotResult = {

    val (initialPossibleValues, data) = dataWithoutEdits(pfs)

    val filtersUpToFirstMarketDataField = pfs.allFilterPaths.chopUpToFirstNon(inMemoryFields)
    val possibleValuesBuilder = new PossibleValuesBuilder(fieldDetails, filtersUpToFirstMarketDataField)

    possibleValuesBuilder.init(initialPossibleValues)

    val editedData = if(edits == PivotEdits.Null) data else {
      data.map { row => {
        val key = row.filterKeys(f => keyFields.contains(f))
        row.map { case (field, value) => {
          edits.editFor(key, field) match {
            case None => {
              field -> value
            }
            case Some((matchedKey, edit)) => {
              field -> edit.applyEdit(matchedKey, field, value)
            }
          }
        } }
      } }
    }

    val allFields = pfs.allFieldsUsed

    val addedRows = edits.newRows.zipWithIndex.map{case (row,index) => {
      val fixedRow = if (marketDataType.marketDataKeyFelds.forall(f => row.contains(f) && row(f) != UndefinedValue)) {
        row ++ marketDataType.createKey(row).fieldValues
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

    val allRows = editedData.toList ::: addedRows

    val result = UnfilteredPivotTableDataSource.applyFiltersAndCalculatePossibleValues(fieldDetails, allRows, pfs)//.removeAll(filtersUpToFirstMarketDataField.allFields))

    PivotResult(result.data, result.possibleValues ++ possibleValuesBuilder.build)
  }
}
