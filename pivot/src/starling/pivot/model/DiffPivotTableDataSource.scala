package starling.pivot.model

import starling.pivot._
import starling.utils.ImplicitConversions._

/**
 * Shows the difference between two pivot reports by adding an extra diff and percentage diff
 * fields for all datafields (ie fields which are quantities)
 *
 * Shows a - b
 */
class DiffPivotTableDataSource(a:PivotTableDataSource, b:PivotTableDataSource, labelB:String) extends PivotTableDataSource {

  /*val fields = {
    a.fields.flatMap { field => {
      if (field.isDataField) {
        List(
          new RawField(true, field),
          new RawField(false, field),
          new DiffField(field),
          new PercentageDiffField(field)
        )
      } else {
        List(field)
      }
    }}
  }*/

  def fieldDetailsGroups = {
    a.fieldDetailsGroups.map(fieldDetailsGroup => {
      if (fieldDetailsGroup.fields.exists(_.isDataField)) {
        val newFields = fieldDetailsGroup.fields.flatMap(field => {
          if (field.isDataField) {
            List(
              new RawField(true, field),
              new RawField(false, field),
              new DiffField(field),
              new PercentageDiffField(field)
              )
          } else {
            List(field)
          }
        })
        fieldDetailsGroup.copy(fields = newFields)
      } else {
        fieldDetailsGroup
      }
    })
  }

  override def initialState = a.initialState.copy(reportSpecificChoices = a.initialState.reportSpecificChoices ++ b.initialState.reportSpecificChoices)

  override def drillDownGroups = a.drillDownGroups

  private val fieldDetailsMap = fieldDetails.asMap(_.field)

  class RawField(val isA:Boolean, val underlyingField:FieldDetails) extends FieldDetails(
    Field( underlyingField.field.name + (if (isA) "" else " " + labelB))) {
    override def isDataField = true
    override def value(a: Any) = underlyingField.value(a)
    override def formatter = DelegatingPivotFormatter(underlyingField.formatter)
    override def combineGroup(groupA: Any, groupB: Any) = underlyingField.combineGroup(groupA, groupB)
    override def combine(group: Any, value: Any) = underlyingField.combine(group, value)
    override def nullValue() = underlyingField.nullValue
    override def nullGroup() = underlyingField.nullGroup
  }
  abstract class AbstractDiffField(name:String, val underlyingField:FieldDetails) extends FieldDetails(Field(name))  {
    def difference(diffGroup:DiffGroup):Any
    override def isDataField = true
    override def value(a: Any) = difference(a.asInstanceOf[DiffGroup])
    override def formatter = AbstractDiffFieldPivotFormatter
    override def combineGroup(groupA: Any, groupB: Any) = groupA.asInstanceOf[DiffGroup].combine(groupB.asInstanceOf[DiffGroup])
    override def combine(group: Any, value: Any) = group.asInstanceOf[DiffGroup].add(value.asInstanceOf[DiffValue])
    override def nullGroup() = new DiffGroup(underlyingField, underlyingField.nullGroup, underlyingField.nullGroup)
    override def nullValue() = new DiffGroup(underlyingField, underlyingField.nullGroup, underlyingField.nullGroup) //don't undestand this
  }
  class DiffField(underlyingField:FieldDetails) extends AbstractDiffField(underlyingField.field.name + " Change", underlyingField)  {
    def difference(diffGroup: DiffGroup) = diffGroup.absoluteDiff
  }
  class PercentageDiffField(underlyingField:FieldDetails) extends AbstractDiffField(underlyingField.field.name + " % Change", underlyingField) {
    def difference(diffGroup: DiffGroup) = diffGroup.percentageDiff
  }
//  def unfilteredData(dataArea: Seq[Field], rowArea: Seq[Field], columnArea: Seq[Field], filters: Seq[(Field, Selection)], reportSpecificChoices : Map[String, String]) = {
  def data(pfs : PivotFieldsState) = {
//    val aResult = dataForUnderlying(true, a, dataAreaFieldDetails, rowArea, columnArea, filters, reportSpecificChoices)
    val aResult = dataForUnderlying(true, a, pfs)
//    val bResult = dataForUnderlying(false, b, dataAreaFieldDetails, rowArea, columnArea, filters, reportSpecificChoices)
    val bResult = dataForUnderlying(false, b, pfs)
    PivotResult(aResult.data ++ bResult.data, union(aResult.possibleValues, bResult.possibleValues))
  }

  private def union(mapA:Map[Field,List[Any]], mapB:Map[Field,List[Any]]):Map[Field,List[Any]] = {
    val allFields = mapA.keysIterator ++ mapB.keysIterator
    Map() ++ allFields.map( f=> f-> {
      val values = mapA.getOrElse(f, List[Any]()) ++ mapB.getOrElse(f, List[Any]())
      (Set() ++ values).toList
    })
  }


  case class DiffGroup(field:FieldDetails, a:Any, b:Any) {
    def add(value:DiffValue) = if (value.isA) DiffGroup(field, field.combine(a, value.value), b) else DiffGroup(field, a, field.combine(b, value.value))
    def combine(other:DiffGroup) = DiffGroup(field, field.combineGroup(a, other.a), field.combineGroup(b, other.b))
    def absoluteDiff = {
      val valueA = field.value(a)
      val valueB = field.value(b)
      (valueA,valueB) match {
        case (a:Int,b:Int) => a - b
        case (a:Double,b:Double) => a - b
        case (a:PivotQuantity,b:PivotQuantity) => a - b
      }
    }
    def percentageDiff = {
      val valueA = field.value(a)
      val valueB = field.value(b)
      (valueA,valueB) match {
        case (a:Int,b:Int) => a - b / b
        case (a:Double,b:Double) => a - b / b
        case (a:PivotQuantity,b:PivotQuantity) => {
          val diff = a.percentageDifference(b)
          diff match {
            case Some(p) => p
            case None => "E"
          }
        }
      }
    }
  }

  private def dataForUnderlying(
          isA:Boolean,
          underlyingDataSource:PivotTableDataSource,
          pfs : PivotFieldsState) = {

    def fix(trees:List[ColumnTree]):List[ColumnTree] = {
//      trees.map { tree => {
//        val fd = fieldDetailsMap.getOrElse(head.field, throw new Exception("No fieldDetails found for " + head.field))
//        (fd match {
//          case r:RawField => if (r.isA == isA) Some(r.underlyingField.field) else None
//          case d:DiffField => Some(d.underlyingField.field)
//          case p:PercentageDiffField => Some(p.underlyingField.field)
//          case _ => Some(head.field)
//        }) match {
//          case None => fix(tail, soFar)
//          case Some(field) => fix(tail, ColumnStructure(field, head.isData, fix(head.children, List())) :: soFar)
//        }
//      } }
      throw new Exception("Fix if this is ever used")
    }
    val fixedUpColumns = pfs.columns.copy(trees=fix(pfs.columns.trees))
    val underlyingResult = underlyingDataSource.data(pfs.copy(columns = fixedUpColumns))
    val dataFields = pfs.columns.measureFields
    val dataFieldDetails = dataFields.map(fieldDetailsMap)
    val data = underlyingResult.data.map {
      row => {
        Map() ++ dataFieldDetails.map { fd=> fd match {
          case r:RawField => if (r.isA == isA)
              fd.field->row(r.underlyingField.field)
            else
              fd.field->r.underlyingField.nullValue
          case d:DiffField => fd.field -> DiffValue(isA, row(d.underlyingField.field))
          case p:PercentageDiffField => fd.field-> DiffValue(isA, row(p.underlyingField.field))
          case _ => fd.field -> row(fd.field)
        }} ++ row.filter( tuple => !dataFields.contains(tuple._1) )
      }
    }
    PivotResult(data, underlyingResult.possibleValues)
  }

}

case class DiffValue(isA:Boolean, value:Any)

case class DelegatingPivotFormatter(formatter:PivotFormatter) extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = formatter.format(value, formatInfo)
}
object AbstractDiffFieldPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    value match {
      case pq:PivotQuantity => TableCell.fromPivotQuantity(pq, formatInfo)
      case other => new TableCell(other)
    }
  }
}