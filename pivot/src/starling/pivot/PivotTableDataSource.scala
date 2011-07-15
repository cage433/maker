package starling.pivot

import controller.PivotTableConverter._
import controller.{PivotTableConverter, PivotGrid, TreePivotFilterNode}
import model.{NoValue, NewRowValue, UndefinedValue, PivotTableModel}
import starling.utils.ImplicitConversions._
import starling.pivot.FilterWithOtherTransform.OtherValue
import starling.pivot.EditableCellState._
import collection.immutable.{Map, TreeMap}
import collection.Set
import org.mockito.internal.matchers.AnyVararg
import collection.script.Start
import reflect.AnyValManifest

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

case class KeyFilter(keys:Map[Field,SomeSelection]) {
  def matches(rowKeys:Map[Field,Any]) = keys.forall{ case (field, selection) => rowKeys.get(field).map(v => selection.values.contains(v)).getOrElse(false) }
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
    true
    //val ks:Set[Field] = matchingKey.keys.keySet.toSet
    //!(ks - deletedField).contains(field)
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

case class PivotEdits(edits:Map[KeyFilter,KeyEdits], newRows:List[Map[Field,Any]]) {
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
  def addEdits(other:PivotEdits):PivotEdits = {
    val newEdits = edits ++ other.edits
    val oneWay = edits.forall{case (k,v) => {
      other.edits.get(k) match {
        case None => true
        case Some(v1) => v == v1
      }
    }}
    val otherWay = other.edits.forall{case (k,v) => {
      edits.get(k) match {
        case None => true
        case Some(v1) => v == v1
      }
    }}
    if (!(oneWay && otherWay)) throw new Exception("All edits must have the same values")

    PivotEdits(newEdits, newRows ++ other.newRows)
  }
  def withDelete(deleteKeys:KeyFilter, field:Field) = {
    val newRowsWithDeleteApplied  = newRows.filterNot(r => deleteKeys.matches(r))
    val editsWithAffectedEditsRemoved = edits.filterKeys { keyFilter => {
      !keyFilter.isOverriddenBy(deleteKeys)
    } }
    PivotEdits(editsWithAffectedEditsRemoved.updated(deleteKeys, DeleteKeyEdit(field)), newRowsWithDeleteApplied)
  }
  def remove(editsToRemove:PivotEdits) = {
    val merged = edits.flatMap{ case (key,changes) => {
      editsToRemove.edits.get(key) match {
        case None => Some(key -> changes)
        case Some(d@DeleteKeyEdit(_)) if changes == d => None
        case Some(d@DeleteKeyEdit(_)) if changes != d => throw new Exception("Can't remove Delete as there is an edit for this key " + key + " " + changes)
        case Some(AmendKeyEdit(amends)) => {
          val newEdit = changes match {
            case DeleteKeyEdit(_) => throw new Exception("Can't remove edit as there is an delete for this key " + key + " " + amends)
            case AmendKeyEdit(oldAmends) => AmendKeyEdit(oldAmends -- amends.keySet)
          }
          if (newEdit.amends.isEmpty) {
            None
          } else {
            Some(key -> newEdit)
          }
        }
      }
    } }
    PivotEdits(merged, newRows.filterNot(editsToRemove.newRows.contains(_)))
  }
  def withNewAmended(rowIndex:Int, field:Field, value:Option[Any]) = {
    val fixedNewRows = newRows.zipWithIndex.map{ case (row,index) => {
      if (index == rowIndex) row.updated(field, value.getOrElse(UndefinedValue)) else row
    }}

    val filteredFixedNewRows = fixedNewRows.filterNot(m => {
      m.forall{case (_,v) => (v == UndefinedValue || v == NoValue)}
    })

    copy(newRows = filteredFixedNewRows)
  }
  def withAmend(amendKeys:KeyFilter, field:Field, value:Option[Any]) = {
    if (newRows.exists(r => amendKeys.matches(r))) {
      val amendedNewRows = newRows.map { r => {
        if (amendKeys.matches(r)) {
          value match {
            case None => r - field
            case Some(v) => r + (field -> v)
          }
        } else {
          r
        }
      }}
      copy(newRows = amendedNewRows)
    } else {
      edits.get(amendKeys) match {
        case None => PivotEdits(edits + (amendKeys -> AmendKeyEdit(Map(field -> value))), newRows)
        case Some(DeleteKeyEdit(_)) => throw new Exception("You can't amend a delete " + (amendKeys, field, value))
        case Some(AmendKeyEdit(amends)) => PivotEdits(edits + (amendKeys -> AmendKeyEdit(amends ++ Map(field -> value))), newRows)
      }
    }
  }

  def withAddedRow(row:Map[Field,Any]) = {
    copy(newRows = newRows ::: List(row))
  }
}
object PivotEdits {
  val Null = PivotEdits(Map.empty, Nil)
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

    def tails[A](list: List[A]) = {
      def recurse(list : List[A]) : List[List[A]] = list match {
        case Nil => Nil
        case _ => list :: recurse(list.tail)
      }

      recurse(list)
    }

    recurse(tails(path.reverse).reverse.map(_.reverse))
  }
}
object PivotTreePath {
  def apply(path:String) = {val b = new PivotTreePath(path.split("/").toList); println(b); b}
}

trait PivotValue {
  def cellType:EditableCellState
  def value:Option[Any] //Current value, None if deleted or undefined in a new row
  def originalValue:Option[Any]
  def edits:PivotEdits
  def valueOrOriginal:Option[Any] = if (value.isDefined) value else originalValue
  def axisValueValue:Any
  def valueForGrouping(newRowsAtBottom:Boolean):Any
}

object PivotValue {
  def create(value:Any) = value match {
    case pv:PivotValue => pv
    case other => StandardPivotValue(other)
  }
  def extractValue(value:Any) = value match {
    case pv:PivotValue => pv.value.getOrElse("DELETED " + pv.originalValue) //FIXME
    case other => other
  }
}

case class StandardPivotValue(value0:Any) extends PivotValue {
  val cellType = Normal
  val value = Some(value0)
  val originalValue = None
  val edits = PivotEdits.Null
  val axisValueValue = value0
  def valueForGrouping(newRowsAtBottom:Boolean) = value0
}

case class DeletedValue(original:Any, edits:PivotEdits) extends PivotValue {
  val cellType = Deleted
  val value = None
  val originalValue = Some(original)
  val axisValueValue = original
  def valueForGrouping(newRowsAtBottom:Boolean) = original
}
case class EditedValue(value0:Any, original:Any, edits:PivotEdits) extends PivotValue {
  val cellType = Edited
  val value = Some(value0)
  val originalValue = Some(original)
  val axisValueValue = value0
  def valueForGrouping(newRowsAtBottom:Boolean) = value0
}
case class NewValue(value:Option[Any], rowIndex:Int, edits:PivotEdits) extends PivotValue {
  val cellType = Added
  val originalValue = None
  val axisValueValue = value.getOrElse(UndefinedValue)
  def valueForGrouping(newRowsAtBottom:Boolean) = if (newRowsAtBottom) NewRowValue(rowIndex) else value.getOrElse(UndefinedValue)
}

case class MeasureCell(value:Option[Any], cellType:EditableCellState, edits:PivotEdits=PivotEdits.Null, originalValue:Option[Any]=None)
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
