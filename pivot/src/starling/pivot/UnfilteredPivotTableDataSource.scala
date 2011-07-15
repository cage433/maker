package starling.pivot

import collection.Seq
import model.UndefinedValue
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}
import starling.pivot.PivotValue


/**
 * Many pivot table data sources aren't able (or willing) to apply filters to their output or collect the distinct
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

class PossibleValuesBuilder(val allFields:Seq[FieldDetails], val filtersList:FiltersList) {
  val fieldDetailsMap = allFields.map(fd => fd.field -> fd).toMap
  private val possibleValues = MutableMap[Field,MutableSet[Any]]()
  for (filters <- filtersList) {
    filters.foreach{ case (field,_) => possibleValues(field) = MutableSet[Any]() }
  }

  def +=(row : Map[Field,Any]) {
    def getFieldValue(field : Field) : Any = {
      val value = row.getOrElse(field, UndefinedValue)
      fieldDetailsMap(field).transformValueForGroupByField(value)
    }
    // Need to add values for all matching selections and the first non-matching
    // (if that exists)
    for (filters <- filtersList) {
      val (matching, nonMatching) = filters.span{case (field, selection) => selection.matches(fieldDetailsMap(field), getFieldValue(field))}
      matching.foreach {
        case (field, _) => possibleValues(field) += getFieldValue(field)
      }
      nonMatching match {
        case Nil =>
        case (field, _) :: _ => possibleValues(field) += getFieldValue(field)
      }
    }
  }
  def build = possibleValues.mapValues(_.toList).toMap
}

object UnfilteredPivotTableDataSource {

  def applyFiltersAndCalculatePossibleValues(fields:List[FieldDetails], data:List[Map[Field,Any]], pfs:PivotFieldsState):PivotResult = {
    val fieldDetailsMap = Map() ++ fields.map(f=>f.field -> f)
    val filterPaths = pfs.allFilterPaths

    val possibleValueFieldList = pfs.allFilterPaths

    val possibleValuesBuilder = new PossibleValuesBuilder(fields, possibleValueFieldList)
    data.foreach {
      row => {
        possibleValuesBuilder += row
      }
    }

    val filteredData = if (pfs.filters.isEmpty) data else data.filter {
      row => {
        pfs.filters.forall{ case(field,selection) => {
          val fieldDetails = fieldDetailsMap(field)
          val rowValue = fieldDetails.transformValueForGroupByField(row.get(field).map(PivotValue.extractValue).getOrElse(UndefinedValue))
          selection match {
            case SomeSelection(values) => fieldDetails.matches(values, rowValue)
            case _ => true
          }
        } }
      }
    }
    PivotResult(filteredData, possibleValuesBuilder.build)
  }
}