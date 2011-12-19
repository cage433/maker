package starling.marketdata

import starling.pivot._
import pivotparsers.ObservationDayPivotParser
import starling.daterange.ObservationTimeOfDay
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._

/**
 * Represents raw market data as pivot data
 *
 * Only shows one type of market data at a time as otherwise we have to show the union of all market data fields
 *
 * The implementation relies on methods on MarketDataType and MarketDataKey for the market data specific behaviour 
 */
object MarketDataPivotTableDataSource {
  val observationTimeField = FieldDetails.list("Observation Time", ObservationTimeOfDay.names)

  val observationDayField = new FieldDetails("Observation Day") {
    override def comparator = super.comparator.reverse
    override def parser = ObservationDayPivotParser
    override def groupValues[T](values: List[T]) = {
      List(ValueGroup("Latest") → values.take(5), ValueGroup("Historic") → values.drop(5)).filter(_._2.nonEmpty)
    }
  }
}

class MarketDataPivotTableDataSource(preBuilt:PrebuiltMarketDataPivotData, edits:PivotEdits) extends PivotTableDataSource {
  val marketDataType = preBuilt.marketDataType

  val fieldDetailsGroups = preBuilt.fieldDetailsGroups
  override def zeroFields = marketDataType.zeroFields.toSet
  override def initialState = preBuilt.initialState
  override def editable = preBuilt.editable

  def data(pfs : PivotFieldsState):PivotResult = {
    val pfsWithAddedKeyFields = editable.fold(editPivot => pfs.addMissingFilters(editPivot.keyFields), pfs)

    val (initialPossibleValues, data) = preBuilt.dataWithoutEdits(pfsWithAddedKeyFields)

    val filtersUpToFirstMarketDataField = pfsWithAddedKeyFields.allFilterPaths.chopUpToFirstNon(preBuilt.inMemoryFields)
    val possibleValuesBuilder = new PossibleValuesBuilder(fieldDetails, filtersUpToFirstMarketDataField, initialPossibleValues)

    val editedData = edits.applyTo(data)
    val allFields = pfsWithAddedKeyFields.allFieldsUsed

    val addedRows = edits.newRows.zipWithIndex.map { case (row, index) => {
      val fixedRow = if (row.isDefined(marketDataType.marketDataKeyFields)) row + marketDataType.fieldValues(row) else row

      allFields.toMapWithValues(field => NewValue(fixedRow.get(field), index, PivotEdits.Null.withAddedRow(fixedRow)))
    } }

    addedRows.foreach { possibleValuesBuilder += _ }

    val allRows = editedData.map(_.value) ::: addedRows

    val result = UnfilteredPivotTableDataSource.applyFiltersAndCalculatePossibleValues(fieldDetails, allRows, pfsWithAddedKeyFields)

    PivotResult(result.data, result.possibleValues ++ possibleValuesBuilder.build)
  }
}