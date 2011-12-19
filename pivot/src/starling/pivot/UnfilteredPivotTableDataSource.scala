package starling.pivot

import collection.immutable.Map
import scalaz.Scalaz._
import starling.utils.ImplicitConversions._

/**
NP * Many pivot table data sources aren't able (or willing) to apply filters to their output or collect the distinct
 * values of those fields. In that case they should derive from this class, which will handle that for them.
 */
abstract class UnfilteredPivotTableDataSource extends PivotTableDataSource {

  lazy val fieldDetailsMap = Map() ++ fieldDetails.map(f=>f.field -> f)

  def unfilteredData(pfs : PivotFieldsState) : List[Map[Field,Any]]

  def data(pfs : PivotFieldsState):PivotResult = {
    //TODO [24 Nov 2010] remove the pfs argument as it shouldn't be needed
    UnfilteredPivotTableDataSource.applyFiltersAndCalculatePossibleValues(fieldDetails, unfilteredData(pfs), pfs)
  }
}

object UnfilteredPivotTableDataSource {

  def applyFiltersAndCalculatePossibleValues(fields:List[FieldDetails], data:List[Map[Field,Any]], pfs:PivotFieldsState):PivotResult = {
    val possibleValuesBuilder = new PossibleValuesBuilder(fields, pfs.allFilterPaths)
    data.foreach { possibleValuesBuilder += _ } // Maybe this isn't adding Market

    val filteredData = if (pfs.filters.isEmpty) data else {
      val filtersByField: Map[FieldDetails, Selection] = pfs.filters.toMap.flatMapKeys(fields.toMapWithKeys(_.field))

      lazy val selectedValuesByField = pfs.filters.flatMap { case (field, selection) => selection partialMatch {
        case someSelection: SomeSelection => field → possibleValuesBuilder.selectedValues[Any](field, someSelection).somes
      } }.toMap

      data.filter { row =>
        // This used to be possibleValueFieldList.fitlers.exists( ... but this means there are sometimes many blank rows when for example looking at one market in the market data viewer)
        filtersByField.forall { case (fieldDetails, selection) => {
          val rowValue = fieldDetails.transformValueForGroupByField(PivotValue.extractValue(row, fieldDetails.field))
          val selectedValuesForField: Option[List[Any]] = selectedValuesByField.get(fieldDetails.field)

          selectedValuesForField.fold(_.contains(rowValue), false) || selection.matches(fieldDetails, rowValue)
        } }
      }
    }

    PivotResult(filteredData, possibleValuesBuilder.build)
  }
}