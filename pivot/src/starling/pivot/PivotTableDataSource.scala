package starling.pivot

import collection.immutable.TreeMap
import controller.PivotTableConverter._
import controller.{PivotTableConverter, PivotGrid, TreePivotFilterNode}
import model.{UndefinedValue, PivotTableModel}
import starling.utils.ImplicitConversions._
import starling.pivot.FilterWithOtherTransform.OtherValue
import starling.pivot.EditableCellState._

case class FieldDetailsGroup(name:String, fields:List[FieldDetails]) {
  def toFieldGroup = {
    FieldGroup(name, fields.map(_.field))
  }
  def fieldMap = fields.map(fd=>fd.field→fd).toMap
}
object FieldDetailsGroup {
  def apply(name: String, fields: FieldDetails*): FieldDetailsGroup = FieldDetailsGroup(name, fields.toList)
}

case class FieldGroup(name:String, fields:List[Field])

object PivotTableDataSource {
  import Field._

//  val ThetaLayout = new PivotFieldsState(
//    rowFields = List(
//      Field(tradeID_str),
//      Field("Risk Type"),
//      Field("Risk Commodity"),
//      Field(riskMarket_str),
//      Field(riskPeriod_str),
//      Field(instrument_str)
//    ),
//    columns = ColumnStructure(ColumnStructure.RootField, false, List(
//      ColumnStructure(Field("Theta"), true, List()),
//      ColumnStructure(Field("Quantity"), true, List())
//    )),
//    reportSpecificChoices = DefaultReportSpecificChoices
//  )
//
//  val PnLLayout = new PivotFieldsState(
//    rowFields = List(
//      Field(tradeID_str),
//      Field("Known/Estimate"),
//      Field("Past/Future"),
//      Field("Asset Delivery Day"),
//      Field("Settlement Market"),
//      Field("Risk Type"),
//      Field("Risk Commodity"),
//      Field(riskMarket_str),
//      Field(riskPeriod_str),
//      Field("UTP Type")
//    ),
//    columns = ColumnStructure(ColumnStructure.RootField, false, List(
//      ColumnStructure(Field("Amount"), true, List()),
//      ColumnStructure(Field("P&L"), true, List()),
//      ColumnStructure(Field("Quantity"), true, List())
//    )),
//    reportSpecificChoices = DefaultReportSpecificChoices
//  )
}

trait PivotEdit {
  val keys:Map[Field,Any]
  def values:Map[Field,Any]
  def value[T](field:Field) = values(field).asInstanceOf[T]
}
case class AmendPivotEdit(keys:Map[Field,Any], measureValues:Map[Field,Any]) extends PivotEdit { //keyFields + editableFields
  def values = keys ++ measureValues
}
case class DeletePivotEdit(keys:Map[Field,Any]) extends PivotEdit {//Just key fields
  def matches(other : Map[Field, Any], ignoredFields : Field*) = {
    val filteredValues = keys -- ignoredFields

    filteredValues.forall{case (field, value) => {
      other.get(field) == Some(value)
    }}
  }
  def values = keys
}

//Measure is editable if all key fields are in row or column or have single filter area selection
//Row is deletable when // TODO [21 Jan 2011] we'll decide later
trait EditPivot {
  def editableToKeyFields:Map[Field,Set[Field]]
  def save(edits:Set[PivotEdit]):Boolean
  def withEdits(edits:Set[PivotEdit]):PivotTableDataSource
}

trait PivotGridSource {
  def gridFor(pivotState: Option[PivotFieldsState]): PivotGrid
}

abstract class PivotTableDataSource extends PivotGridSource {
  lazy val fieldDetails:List[FieldDetails] = fieldDetailsGroups.flatMap(_.fields)
  def fieldDetailsGroups:List[FieldDetailsGroup]
  def data(pfs : PivotFieldsState): PivotResult
  def drillDownGroups:List[DrillDownInfo] = List()
  def initialState: PivotFieldsState = PivotFieldsState()
  def lookup(field: Field) = fieldDetails.find(_.field == field).get
  def lookup(fieldName: String): FieldDetails = lookup(Field(fieldName))

  def parseFilter(field: Field, values: List[String]) =
    (field, SomeSelection(values.map(value => lookup(field).parser.parse(value)._1).toSet))

  def editable:Option[EditPivot] = None
  def availablePages:List[String] = List()
  def reportSpecificOptions : List[(String, List[Any])] = Nil
  def zeroFields:Set[Field] = Set()

  def gridFor(pivotState: Option[PivotFieldsState]) = {
    PivotTableConverter(table = PivotTableModel.createPivotTableData(this, pivotState)).createGrid()
  }

  protected def fieldDetails(names: String*) = names.toList.map(FieldDetails(_))
  protected def fields(fieldDetails: FieldDetails*): List[Field] = fieldDetails.map(_.field).toList
  protected def fields(fieldDetails: List[FieldDetails]) = fieldDetails.map(_.field).toList
  protected def fields(map: (FieldDetails, Any)*) = map.toMap.mapKeys(_.field)
}

case class PivotTreePath(path:List[String]) {
  def isOther = path.forall(_==FilterWithOtherTransform.Other.toString)
  def between(start:Int,end:Int) : List[PivotTreePath] = {
    (for (i <- start to end) yield { if (path.size <= i) {
      PivotTreePath(path)
    } else {
      val p = path.slice(0, i+1)
      PivotTreePath(p)
    } }).toList
  }
  def size = path.size
  def equalOrParentOf(other:PivotTreePath) = {
    if (path.size > other.size) {
      false
    } else {
      val chopped = other.path.slice(0, path.size)
      path == chopped
    }
  }
  override def toString = path.mkString("/")
  def lastElement = {
    if (path.nonEmpty) {
      path.last
    } else {
      ""
    }
  }
  def toTree : TreePivotFilterNode = {
    def recurse(pathPrefixes : List[List[String]]) : TreePivotFilterNode = pathPrefixes match {
      case List(last) => TreePivotFilterNode(PivotTreePath(last), last.last, List())
      case first :: rest => TreePivotFilterNode(PivotTreePath(first), first.last, List(recurse(rest)))
    }

    recurse(path.inits.toList)
  }
}
object PivotTreePath {
  def apply(path:String) = {val b = new PivotTreePath(path.split("/").toList); println(b); b}
}

trait PivotValue {
  def cellType:EditableCellState
  def value:Option[Any]
  def originalValue:Option[Any]
  def edit:Option[PivotEdit]
}

case class StandardPivotValue(value0:Any) extends PivotValue {
  val cellType = Normal
  val value = Some(value0)
  val originalValue = None
  val edit = None
}

case class DeletedValue(original:Any, e:PivotEdit) extends PivotValue {
  val cellType = Deleted
  val value = None
  val edit = Some(e)
  val originalValue = Some(original)
}
case class EditedValue(value0:Any, original:Any, e:PivotEdit) extends PivotValue {
  val cellType = Edited
  val value = Some(value0)
  val edit = Some(e)
  val originalValue = Some(original)
}
case class NewValue(value0:Any, e:PivotEdit) extends PivotValue {
  val cellType = Added
  val value = Some(value0)
  val edit = Some(e)
  val originalValue = None
}

case class MeasureCell(value:Option[Any], cellType:EditableCellState, edits:Set[PivotEdit]=Set.empty)
object MeasureCell {
  val Null = MeasureCell(None, Normal)
  val Undefined = MeasureCell(Some(UndefinedValue), Normal)
}

case class PivotResult(data:Seq[Map[Field,Any]], possibleValues:Map[Field,List[Any]]){
  def numberOfRows = data.size
}

object NullPivotTableDataSource extends PivotTableDataSource {
  val fieldDetailsGroups = List()
  def data(pfs : PivotFieldsState) = new PivotResult(Seq(), Map())
  def options: scala.collection.mutable.Map[String, AnyRef] = new scala.collection.mutable.HashMap[String, AnyRef]()
}
case class PivotAxis(dataFields:List[Field], rowFields:List[Field], columnFields:List[Field], removeDataFields:Boolean) {
  def removeAll(fields:List[Field]) = PivotAxis(dataFields.filterNot(fields.contains), rowFields.filterNot(fields.contains), columnFields.filterNot(fields.contains), removeDataFields)
  def isEmpty = dataFields.isEmpty && rowFields.isEmpty && columnFields.isEmpty
}

case class FilteredDrillDown(filterField:Field, fieldToFields:List[(String,List[Field])])
case class DrillDownInfo(fallBack:PivotAxis, filteredDrillDown:Option[FilteredDrillDown] = None)
