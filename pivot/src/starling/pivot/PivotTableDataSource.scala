package starling.pivot

import controller.PivotTableConverter._
import controller.{PivotTableConverter, PivotGrid, TreePivotFilterNode}
import model.{UndefinedValue, PivotTableModel}
import starling.utils.ImplicitConversions._
import starling.pivot.FilterWithOtherTransform.OtherValue
import starling.pivot.EditableCellState._
import collection.immutable.{Map, TreeMap}
import collection.Set

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

case class KeyFilter(keys:Map[Field,Any]) {
  def matches(rowKeys:Map[Field,Any]) = keys.forall{ case (field, value) => rowKeys(field) == value }
  def isOverriddenBy(newDelete:KeyFilter) = {
    newDelete.keys.forall{ case (key,value) => {
      keys.get(key) == Some(value)
    } }
  }
}

trait KeyEdits {
  def affects(matchingKey:KeyFilter, field:Field):Boolean
  def applyEdit(key:KeyFilter, field:Field, value:Any):Any
}
case class DeleteKeyEdit(deletedField:Field) extends KeyEdits {
  def affects(matchingKey:KeyFilter, field:Field) = {
    val ks:Set[Field] = matchingKey.keys.keySet.toSet
    !(ks - deletedField).contains(field)
  }
  def applyEdit(key:KeyFilter, field:Field, value:Any) = DeletedValue(value, PivotEdits.Null.withDelete(key, deletedField))
}
case class AmendKeyEdit(amends:Map[Field,Option[Any]]) extends KeyEdits {
  def affects(matchingKey:KeyFilter, field:Field) = amends.contains(field)
  def applyEdit(key:KeyFilter, field:Field, value:Any) = {
    amends.get(field) match {
      case None => value
      case Some(None) => DeletedValue(value, PivotEdits.Null.withAmend(key, field, None))
      case Some(Some(newValue)) => EditedValue(newValue, value, PivotEdits.Null.withAmend(key, field, Some(newValue)))
    }
  }
}

case class PivotEdits(edits:Map[KeyFilter,KeyEdits], newRows:Set[Map[Field,Any]]) {
  def editFor(key:Map[Field,Any], field:Field) = {
    val filterKeys:Map[KeyFilter, KeyEdits] = edits.filterKeys(_.matches(key))
    filterKeys.toList match {
      case Nil => None
      case many => {
        val editsForField = many.filter { case (matchedKey,edit) => edit.affects(matchedKey, field) }
        if (editsForField.size > 1) throw new Exception("More than one edit matches " + editsForField)
        editsForField.headOption
      }
    }
  }
  def nonEmpty:Boolean = edits.nonEmpty || newRows.nonEmpty
  def isEmpty:Boolean = !nonEmpty
  def ++(other:PivotEdits):PivotEdits = {
    val merged = edits.map{ case (key,changes) => {
      other.edits.get(key) match {
        case None => key -> changes
        case Some(d@DeleteKeyEdit(field)) => key -> d
        case Some(AmendKeyEdit(amends)) => key -> {
          changes match {
            case DeleteKeyEdit(_) => throw new Exception("Can't edit after a delete")
            case AmendKeyEdit(oldAmends) => AmendKeyEdit(oldAmends ++ amends)
          }
        }
      }
    } }
    PivotEdits(merged, newRows ++ other.newRows)
  }
  def withDelete(deleteKeys:KeyFilter, field:Field) = {
    val editsWithAffectedEditsRemoved = edits.filterKeys { keyFilter => {
      !keyFilter.isOverriddenBy(deleteKeys)
    } }
    PivotEdits(editsWithAffectedEditsRemoved.updated(deleteKeys, DeleteKeyEdit(field)), newRows)
  }
  def remove(editsToRemove:PivotEdits) = {
    val merged = edits.flatMap{ case (key,changes) => {
      editsToRemove.edits.get(key) match {
        case None => Some(key -> changes)
        case Some(d@DeleteKeyEdit(_)) if changes == d => None
        case Some(d@DeleteKeyEdit(_)) if changes != d => throw new Exception("Can't remove Delete as there is an edit for this key " + key + " " + changes)
        case Some(AmendKeyEdit(amends)) => Some(key -> {
          changes match {
            case DeleteKeyEdit(_) => throw new Exception("Can't remove edit as there is an delete for this key " + key + " " + amends)
            case AmendKeyEdit(oldAmends) => AmendKeyEdit(oldAmends -- amends.keySet)
          }
        })
      }
    } }
    PivotEdits(merged, newRows -- editsToRemove.newRows)
  }
  def withAmend(amendKeys:KeyFilter, field:Field, value:Option[Any]) = {
    edits.get(amendKeys) match {
      case None => PivotEdits(edits + (amendKeys -> AmendKeyEdit(Map(field -> value))), newRows)
      case Some(DeleteKeyEdit(_)) => throw new Exception("You can't amend a delete " + (amendKeys, field, value))
      case Some(AmendKeyEdit(amends)) => PivotEdits(edits + (amendKeys -> AmendKeyEdit(amends ++ Map(field -> value))), newRows)
    }
  }
}
object PivotEdits {
  val Null = PivotEdits(Map.empty, Set.empty)
}

//Measure is editable if all key fields are in row or column or have single filter area selection
//Row is deletable when // TODO [21 Jan 2011] we'll decide later
trait EditPivot {
  def editableToKeyFields:Map[Field,Set[Field]]
  def save(edits:PivotEdits):Boolean
  def withEdits(edits:PivotEdits):PivotTableDataSource
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
  def zeroFields:Set[Field] = Set.empty

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
    def recurse(pathPrefixes : List[List[String]]) : TreePivotFilterNode = (pathPrefixes : @unchecked) match {
      case List(last) => TreePivotFilterNode(PivotTreePath(last), last.last, List())
      case first :: rest => TreePivotFilterNode(PivotTreePath(first), first.last, List(recurse(rest)))
    }

    recurse(path.inits.toList.dropRight(1))
  }
}
object PivotTreePath {
  def apply(path:String) = {val b = new PivotTreePath(path.split("/").toList); println(b); b}
}

trait PivotValue {
  def cellType:EditableCellState
  def value:Option[Any]
  def originalValue:Option[Any]
  def edits:PivotEdits
}

object PivotValue {
  def create(value:Any) = value match {
    case pv:PivotValue => pv
    case other => StandardPivotValue(other)
  }
}

case class StandardPivotValue(value0:Any) extends PivotValue {
  val cellType = Normal
  val value = Some(value0)
  val originalValue = None
  val edits = PivotEdits.Null
}

case class DeletedValue(original:Any, edits:PivotEdits) extends PivotValue {
  val cellType = Deleted
  val value = None
  val originalValue = Some(original)
}
case class EditedValue(value0:Any, original:Any, edits:PivotEdits) extends PivotValue {
  val cellType = Edited
  val value = Some(value0)
  val originalValue = Some(original)
}
case class NewValue(value0:Any, edits:PivotEdits) extends PivotValue {
  val cellType = Added
  val value = Some(value0)
  val originalValue = None
}

case class MeasureCell(value:Option[Any], cellType:EditableCellState, edits:PivotEdits=PivotEdits.Null)
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
