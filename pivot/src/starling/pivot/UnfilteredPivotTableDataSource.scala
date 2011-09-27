package starling.pivot

import collection.Seq
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}
import collection.immutable.Map


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

  def init(values:Map[Field, List[Any]]) {
    values.foreach { case (field,values) => {
      possibleValues(field) ++= values
    }}
  }

  def +=(row : Map[Field,Any]) {
    def getFieldValue(field : Field, isForPossibleValues: Boolean) : Any = {
      val value = PivotValue.extractValue(row, field)
      if(isForPossibleValues)
        fieldDetailsMap(field).transformValueForGroupByField(value)
      else
        value
    }
    // Need to add values for all matching selections and the first non-matching
    // (if that exists)
    for (filters <- filtersList) {
      val (matching, nonMatching) = filters.span{case (field, selection) => {
        fieldDetailsMap.get(field) match {
          case None => false
          case Some(fd) => selection.matches(fd, getFieldValue(field, false))
        }
      } }
      matching.foreach {
        case (field, MeasurePossibleValuesFilter(_)) =>
        case (field, _) => possibleValues(field) += getFieldValue(field, true)
      }
      nonMatching match {
        case Nil =>
        case (field, MeasurePossibleValuesFilter(_)) :: _ =>
        case (field, _) :: _ => if (fieldDetailsMap.contains(field)) possibleValues(field) += getFieldValue(field, true)
      }
    }
  }
  def build:Map[Field, List[Any]] = possibleValues.mapValues(_.toList).toMap
}

object UnfilteredPivotTableDataSource {

  def applyFiltersAndCalculatePossibleValues(fields:List[FieldDetails], data:List[Map[Field,Any]], pfs:PivotFieldsState):PivotResult = {
    val fieldDetailsMap = Map() ++ fields.map(f=>f.field -> f)

    val possibleValueFieldList = pfs.allFilterPaths

    val possibleValuesBuilder = new PossibleValuesBuilder(fields, possibleValueFieldList)
    data.foreach {
      row => {
        possibleValuesBuilder += row
      }
    }

    val filteredData = if (pfs.filters.isEmpty) data else data.filter {
      row => {
        possibleValueFieldList.filters.exists { filters => {
          filters.forall{ case(field,selection) => {
            fieldDetailsMap.get(field) match {
              case Some(fieldDetails) => {
                val rowValue = fieldDetails.transformValueForGroupByField(PivotValue.extractValue(row, field))
                selection.matches(fieldDetails, rowValue)
              }
              case None => true
            }
          } }
        } }
      }
    }
    PivotResult(filteredData, possibleValuesBuilder.build)
  }
}