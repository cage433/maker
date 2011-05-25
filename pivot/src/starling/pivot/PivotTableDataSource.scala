package starling.pivot

import collection.immutable.TreeMap
import controller.PivotTableConverter._
import controller.{PivotTableConverter, PivotGrid, TreePivotFilterNode}
import model.PivotTableModel
import starling.utils.ImplicitConversions._
case class FieldDetailsGroup(name:String, fields:List[FieldDetails]) {
  def toFieldGroup = {
    FieldGroup(name, fields.map(_.field))
  }
  def fieldMap = fields.map(fd=>fd.field→fd).toMap
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
  val values:Map[Field,Any]

  def value[T](field : Field) : T = values(field).asInstanceOf[T]
}
case class AmendPivotEdit(values:Map[Field,Any]) extends PivotEdit //keyFields + editableFields
case class DeletePivotEdit(values:Map[Field,Any]) extends PivotEdit {//Just key fields
  def matches(other : Map[Field, Any], ignoredFields : Field*) = {
    val filteredValues = values -- ignoredFields

    filteredValues.forall{case (field, value) => {
      other.get(field) == Some(value)
    }}
  }
}

//Measure is editable if all key fields are in row or column or have single filter area selection
//Row is deletable when // TODO we'll decide later
trait EditPivot {
  def editableToKeyFields:Map[Field,Set[Field]]
  def save(edits:Set[PivotEdit]):Boolean
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

  def editable:Option[EditPivot] = None
  def availablePages:List[String] = List()
  def reportSpecificOptions : List[(String, List[Any])] = Nil

  def gridFor(pivotState: Option[PivotFieldsState]) = {
    PivotTableConverter(table = PivotTableModel.createPivotTableData(this, pivotState)).createGrid()
  }

  protected def fields(fieldDetails: FieldDetails*) = fieldDetails.map(_.field).toList
  protected def fields(fieldDetails: List[FieldDetails]) = fieldDetails.map(_.field).toList
  protected def fields(map: (FieldDetails, Any)*) = map.toMap.mapKeys(_.field)
}

case class PivotTreePath(path:List[String]) {
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

    recurse(path.inits)
  }
}
object PivotTreePath {
  def apply(path:String) = {val b = new PivotTreePath(path.split("/").toList); println(b); b}
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
