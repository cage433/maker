package starling.pivot

import java.io.Serializable
import starling.utils.StarlingObject
import starling.pivot.ColumnStructure._
import collection.immutable.TreeMap
import collection.SortedMap
import starling.utils.ImplicitConversions._


object FieldChooserType extends Enumeration {
  type FieldChooserType = Value
  val Filter = Value("Filter")
  val Rows = Value("Rows")
  val Columns = Value("Columns")
  val FieldList = Value("Field List")
}

import starling.pivot.FieldChooserType._

trait Selection extends Serializable{
  def matches(fieldDetails:FieldDetails, value : Any) : Boolean
  def description: String
}

object AllSelection extends Selection with StarlingObject {
  private def readResolve = AllSelection
  override def toString = "AllSelection"

  def matches(fieldDetails:FieldDetails, value: Any) = true
  def description = "All"
}
case class SomeSelection(values:Set[Any]) extends Selection{
  def matches(fieldDetails:FieldDetails, value: Any) = fieldDetails.matches(values, value)
  def description = values.mkString(", ").replaceLast(", ", ", or ")
}

case class GreaterThanSelection(selection: Comparable[_]) extends Selection{
  def matches(fieldDetails:FieldDetails, value: Any) = value match {
    case c: Comparable[_] => c.asInstanceOf[Comparable[Object]].compareTo(selection.asInstanceOf[Comparable[Object]]) > 0
    case _ => throw new Exception("Can't use non-comparable value: " + value)
  }
  def description = "Greater than: " + selection
}

case class Totals(rowGrandTotal:Boolean, rowSubTotals:Boolean, columnGrandTotal:Boolean, columnSubTotals:Boolean) {
  def toggleRowGrandTotal = copy(rowGrandTotal = !rowGrandTotal)
  def toggleColumnGrandTotal = copy(columnGrandTotal = !columnGrandTotal)
  def toggleRowSubTotals = copy(rowSubTotals = !rowSubTotals)
  def toggleColumnSubTotals = copy(columnSubTotals = !columnSubTotals)
  def noSubTotals = copy(rowSubTotals=false, columnSubTotals=false)
}
object Totals {
  def Null = Totals(false, false, false, false)
}

object Position extends Enumeration {
  type Position = Value
  val Top,Left,Bottom,Right,Other = Value
}

import Position._

trait HasChildren[T] {
  def children : List[T]
}

case class FiltersList(filters:List[List[(Field,Selection)]]) extends Iterable[List[(Field,Selection)]] {
  def toFilterSet = filters.flatten.toSet
  def allFields = toFilterSet.map(_._1)
  def iterator = filters.iterator
  def chopUpToFirstNon(fields:Set[Field]) = {
    val chopped = filters.map {
      path => {
        val (before,after) = path.span{ case(f,_)=>fields.contains(f) }
        before
      }
    }
    FiltersList(chopped)
  }
}

case class FilterWithOtherTransform(selection:Set[Any])
object FilterWithOtherTransform {
  case object OtherValue { override def toString = "Other" }
  val Other = OtherValue
  def treeNode = (OtherValue, OtherValue.toString)
}


case class ColumnStructurePath(dataField:Option[Field], path:List[(Field, Int)]) {
  def addField(field:Field, index:Int) = ColumnStructurePath(dataField, (field, index) :: path)

  def addDataField(field:Field, index:Int) = dataField match {
    case None => ColumnStructurePath(Some(field), (field, index) :: path)
    case Some(df) => throw new Exception("There are two datafields in the path " + df + " and " + field)
  }
}

object ColumnStructure {
  val TempField = Field("temp")
  val RootField = Field("ROOT")
  def rootChildren(children:List[ColumnStructure]) = ColumnStructure(RootField, false, children)
  def dataField(field:Field) = ColumnStructure(field, true, List())

  def createFlat(columnFields:List[Field], dataFields:List[Field]) = {
    var cses = dataFields.map(f=>ColumnStructure(f, true, List()))
    columnFields.reverse.foreach(f=> cses = List(ColumnStructure(f, false, cses)))
    ColumnStructure(RootField, false, cses)
  }

  def buildPathsFor(children:List[ColumnStructure]):List[ColumnStructurePath] = {
    children.zipWithIndex.flatMap {case (cs, index) => buildPaths(cs, index)}
  }

  private def buildPaths(cs:ColumnStructure, position:Int):List[ColumnStructurePath] = {
    cs match {
      case ColumnStructure(field, true, Nil) => List(ColumnStructurePath(Some(field), (field, position) :: Nil))
      case ColumnStructure(field, false, Nil) => List(ColumnStructurePath(None, (field, position) :: Nil))
      case ColumnStructure(field, true, children) => buildPathsFor(children).map(_.addDataField(field, position))
      case ColumnStructure(field, false, children) => buildPathsFor(children).map(_.addField(field, position))
    }
  }
}

case class ColumnStructure(field:Field, isData:Boolean, children:List[ColumnStructure]=Nil) extends HasChildren[ColumnStructure] {
  def allFields:List[Field] = (field :: children.toList.flatMap(cs => cs.allFields)).filterNot(_ == RootField)
  def columnFields:List[Field] = ((if (isData) None else Some(field)).toList ::: children.toList.flatMap(cs => cs.columnFields)).filterNot(_ == RootField)
  def dataFields:List[Field] = (if (isData) Some(field) else None).toList ::: children.toList.flatMap(cs => cs.dataFields)
  def hasColumnField = !isData
  def keep(fields:Set[Field]):ColumnStructure = {
    ColumnStructure(field, isData, children.filter(child => fields.contains(child.field)).map(_.keep(fields)))
  }
  def addParents(columns:List[Field]):ColumnStructure = {
    columns.reverse match {
      case Nil => this
      case head :: tail => addParent(head).addParents(tail.reverse)
    }
  }
  def isEmpty = children.isEmpty
  def addParent(parent:Field) = {
    ColumnStructure(parent, false, List(this))
  }
  private def addChild(field:Field, addIsData:Boolean):ColumnStructure = {
    copy(children = children ::: List(ColumnStructure(field, addIsData, List())))
  }
  def addDataField(dataField:Field):ColumnStructure = {
    children match {
      case Nil => addChild(dataField, true)
      case other if other.exists(_.isData) => addChild(dataField, true)
      case head :: tail => copy(children = head.addDataField(dataField) :: tail)
    }    
  }
  def add(newField:Field, newIsData:Boolean, relativeTo:Field, position:Position):ColumnStructure = {
    position match {
      case Top if relativeTo == field => ColumnStructure(newField, newIsData, List( this ))
      case Bottom if relativeTo == field=> ColumnStructure(field, isData, List( ColumnStructure(newField, newIsData, children)))
      case Left | Right if children.map(_.field).contains(relativeTo) => {
        val newStructure = ColumnStructure(newField, newIsData, List())
        ColumnStructure(field, isData,  children.flatMap {
          child => {
            if (child.field == relativeTo) {
              if (position == Left) {
                List(newStructure, child)
              } else {
                List(child, newStructure)
              }
            } else {
              List(child)
            }
          }
        })
      }
      case _ => ColumnStructure(field, isData, children.map(_.add(newField, newIsData, relativeTo, position)))
    }
  }
  def contains(testField:Field):Boolean = {
    if (field == testField) {
      true
    } else {
      children.foldLeft(false)((sum,el) => if (sum) sum else el.contains(testField))
    }
  }
  def removeAllChildren(fields:Set[Field]):ColumnStructure = {
    def recursiveRemove(fields:Set[Field], columns:List[ColumnStructure]):List[ColumnStructure] = {
      columns.flatMap {
        child => {
          if (fields.contains(child.field)) {
            recursiveRemove(fields, child.children)
          } else {
            List(child.removeAllChildren(fields))
          }
        }
      }
    }
    ColumnStructure(field, isData, recursiveRemove(fields, children))
  }
  def remove(removeField:Field):ColumnStructure = {
    assert(removeField != RootField, "You cannot remove the root node")

    val childFields = children.map(_.field)
    if (childFields.contains(removeField)) {
      val newChildren = children.splitAt(childFields.indexOf(removeField)) match {case (l,r) => l ::: r.head.children ::: r.tail}
      ColumnStructure(field, isData, newChildren)
    } else {
      ColumnStructure(field, isData, children.map(_.remove(removeField)))
    }
  }
  def replace(fieldToReplace:Field, newField:Field):ColumnStructure = {
    if (field == fieldToReplace) {
      ColumnStructure(newField, isData, children)
    } else {
      val childFields = children.map(_.field)
      if (childFields.contains(fieldToReplace)) {
        val newChildren = children.splitAt(childFields.indexOf(fieldToReplace)) match {
          case (l,r) => l ::: ColumnStructure(newField, r.head.isData, r.head.children) :: r.tail
        }
        ColumnStructure(field, isData, newChildren)
      } else {
        ColumnStructure(field, isData, children.map(_.replace(fieldToReplace, newField)))
      }
    }
  }

  def flipIsData(fieldToFlip:Field):ColumnStructure = {
    if (field == fieldToFlip) {
      ColumnStructure(field, !isData, children)
    } else {
      val childFields = children.map(_.field)
      if (childFields.contains(fieldToFlip)) {
        val newChildren = children.splitAt(childFields.indexOf(fieldToFlip)) match {
          case (l,r) => l ::: ColumnStructure(r.head.field, !r.head.isData, r.head.children) :: r.tail
        }
        ColumnStructure(field, isData, newChildren)
      } else {
        ColumnStructure(field, isData, children.map(_.flipIsData(fieldToFlip)))
      }
    }
  }

  def isInvalid():Boolean = {
    if (isData) {
      !(children.flatMap(cs => cs.dataFields)).isEmpty
    } else {
      children.exists(_.isInvalid)
    }
  }
  
  def hasPathContaining(fields:Set[Field]) : Boolean = {
    if (fields.isEmpty){
      true
    } else if (fields == Set(field)){
      true
    } else if (fields.contains(field)){
      children.exists(_.hasPathContaining(fields - field))
    } else {
      children.exists(_.hasPathContaining(fields))
    }
  }

  def isBottomField(f:Field):Boolean = {
    if (children.isEmpty) {
      f == field
    } else {
      children.exists(_.isBottomField(f))
    }
  }
}

//This was a case class but to ensure compatability with old code it is now a normal
//class where the object has an apply method matching the old constructor signature
//for that reason there are many case like methods (ie copy, equals and hashCode)
class PivotFieldsState(
        val rowFields:List[Field]=List(),
        val columns:ColumnStructure=ColumnStructure(RootField, false, List()),
        val filters:List[(Field,Selection)]=List(),
        val treeDepths:SortedMap[Field,(Int,Int)]=TreeMap.empty,
        val reportSpecificChoices:SortedMap[String,Any]=TreeMap.empty,
        val transforms:SortedMap[Field,FilterWithOtherTransform]=TreeMap.empty
  ) extends Serializable {

  assert(filters.size == filters.map(_._1).toSet.size, {Thread.dumpStack; "There are duplicated filter fields"})

  override def toString = "r: " + rowFields + " c: " + columns + " f: " + filters  + " treeDep: " +
          treeDepths + " rpc: " + reportSpecificChoices
  def allFieldsUsed = columns.allFields ::: rowFields ::: filters.map(_._1)

  def filterAreaFields = {
    filters.map(_._1).filterNot{ f=> columns.contains(f) || rowFields.contains(f)}
  }

  def filtersInTheFilterArea = {
    filters.filterNot{ case(f,_) => columns.contains(f) || rowFields.contains(f) }
  }

  def copy(
                  rowFields:List[Field] = rowFields,
                  columns:ColumnStructure = columns,
                  reportSpecificChoices:SortedMap[String,Any]=reportSpecificChoices,
                  filters:List[(Field,Selection)]=filters,
                  treeDepths:SortedMap[Field,(Int,Int)]=treeDepths,
                  transforms:SortedMap[Field,FilterWithOtherTransform]=transforms) = {
    new PivotFieldsState(rowFields, columns, filters, treeDepths, reportSpecificChoices, transforms)
  }

  override def equals(o: Any) = o match {
    case other : PivotFieldsState => {
      this.rowFields == other.rowFields &&
      this.columns == other.columns &&
      this.filters == other.filters &&
      this.treeDepths == other.treeDepths &&
      this.reportSpecificChoices == other.reportSpecificChoices &&
      this.transforms == other.transforms
    }
    case _ => false
  }

  override def hashCode = 1 

  def withFiltersAndRowFields(newFilters:Seq[(Field,Selection)], newAxisFields:PivotAxis) = {
    val dataFieldsToUse = if (newAxisFields.removeDataFields) {
      newAxisFields.dataFields
    } else {
      columns.dataFields ::: newAxisFields.dataFields
    }
    val newFilterFields = newFilters.map(_._1).toSet
    new PivotFieldsState(
      (rowFields.filterNot(newFilters.toList.map(_._1).contains)) ::: newAxisFields.rowFields,
      ColumnStructure.createFlat(newAxisFields.columnFields, dataFieldsToUse),
      filters.filterNot(f=> newFilterFields.contains(f._1)) ::: newFilters.toList,
      treeDepths,
      reportSpecificChoices,
      transforms
    )
  }
  def rotate = new PivotFieldsState(columns.columnFields, ColumnStructure.createFlat(rowFields, columns.dataFields), filters, treeDepths, reportSpecificChoices, transforms)
  def hasRowOrColumnFields = !rowFields.isEmpty || columns.hasColumnField
  def moveField(field:Field, from:FieldChooserType, to:FieldChooserType, pos:Int):PivotFieldsState = {
    val newFilterFields = if (to == FieldList) {
      filters.filter(_._1 != field)
    } else if (to == Filter) {
      val entry = filters.find{case (f,_) => f==field}.getOrElse( (field,AllSelection) )
      val filtersToFilter = filters.filterNot{case (_field,_) => _field==field}
      filtersToFilter.splitAt(pos) match {case (l1, l2) => l1 ::: entry :: l2}
    } else {
      filters
    }

    val cleanRows = if (Rows == from) rowFields.filterNot(_ == field) else rowFields
    val newRowFields = if (Rows == to) cleanRows.splitAt(pos) match { case (l1, l2) => l1 ::: field :: l2} else cleanRows

    val cleanColumns = if (Columns == from) columns.remove(field) else columns
    val newColumns = if (Columns == to) cleanColumns.addDataField(field) else cleanColumns

    val newTransforms = transforms.filter(t => newColumns.contains(t._1) || newRowFields.contains(t._1))

    new PivotFieldsState(newRowFields, newColumns, newFilterFields, treeDepths, reportSpecificChoices, newTransforms)
  }

  def moveField(field:Field, from:FieldChooserType, newColumnStructure:ColumnStructure):PivotFieldsState = {
    // If the field has been changed into a measure field, remove any filters that it might have.
    val newFilters = if (newColumnStructure.dataFields.contains(field)) {
      filters.filterNot{case (_field, _) => _field == field}
    } else {
      filters
    }

    val rFields = if (Rows == from) {
      rowFields.filter(_ != field)
    } else {
      rowFields
    }

    val newTransforms = transforms.filter(t => newColumnStructure.contains(t._1) || rFields.contains(t._1))

    new PivotFieldsState(rFields, newColumnStructure, newFilters, treeDepths, reportSpecificChoices, newTransforms)
  }

  def mapSelectionValues(f:Any=>Any) = {
    val modifiedFilters = filters.map { case (field,selection) => {
      selection match {
        case SomeSelection(values) => {
          field -> SomeSelection(values.map(f))
        }
        case _ => field -> selection
      }
    }}
    copy(filters = modifiedFilters)
  }


  def removeAll(fields:Set[Field]) = {
    new PivotFieldsState(
      rowFields.filterNot(fields.contains(_)),
      columns.removeAllChildren(fields),
      filters.filterNot(f=>fields.contains(f._1)),
      treeDepths,
      reportSpecificChoices,
      transforms.filterNot(f=>fields.contains(f._1))
    )
  }

  def keepAll(validFields:Set[Field]) = {
    new PivotFieldsState(
      rowFields.filter(validFields.contains(_)),
      columns.keep(validFields),
      filters.filter(f=>validFields.contains(f._1)),
      treeDepths,
      reportSpecificChoices,
      transforms.filter(f=>validFields.contains(f._1))
    )
  }

  def addFilter(filter:(Field,Set[Any])) = {
    val newFilters = if (filters.map(_._1).contains(filter._1)) {
      filters.map { case (field,selection) => {
        if (field == filter._1) {
          field -> new SomeSelection(filter._2)
        } else {
          field -> selection
        }
      } }
    } else {
      filter._1 -> SomeSelection(Set(filter._2)) :: filters
    }
    copy(filters=newFilters)
  }

  def allFilterPaths = {
    val columnPaths = ColumnStructure.buildPathsFor(columns.children).map(path => {
      path.path.map(_._1).filter(f => {
        path.dataField match {
          case None => true
          case Some(df) => df != f
        }
      })
    })
    val allPaths: List[scala.List[Field]] = if (rowFields.isEmpty) {
      if (columnPaths.isEmpty) {
        List(Nil)
      } else {
        columnPaths
      }
    } else {
      rowFields :: columnPaths.map(p=>rowFields:::p)
    }

    val filterFieldToSelection = Map() ++ filters.map(t=>t._1->t._2)

    val filtersList = allPaths.map(path => filtersInTheFilterArea ::: path.map {
      field => {
        val selection = filterFieldToSelection.get(field) match {
          case Some(s) => s
          case None => AllSelection
        }
        (field, selection)
      }
    })
    FiltersList(filtersList)
  }

  def fieldSelection(f:Field) = {
    filters.find{ case (field,selection) => field==f && selection.isInstanceOf[SomeSelection]}.map { case (_,SomeSelection(values)) => values }
  }

  def addChoice(choice : (String, Any)) = copy(reportSpecificChoices = reportSpecificChoices + choice)
}

object PivotFieldsState {
  def apply(
        dataFields:List[Field]=List(),
        rowFields:List[Field]=List(),
        columnFields:List[Field]=List(),
        filters:List[(Field,Selection)]=List(),
        treeDepths:SortedMap[Field,(Int,Int)]=TreeMap.empty,
        reportSpecificChoices : SortedMap[String, Any] = TreeMap.empty,
        transforms : SortedMap[Field, FilterWithOtherTransform] = TreeMap.empty
  ) = {
    new PivotFieldsState(
      columns=ColumnStructure.createFlat(columnFields, dataFields),
      rowFields=rowFields,
      filters=filters,
      treeDepths=treeDepths,
      reportSpecificChoices=reportSpecificChoices,
      transforms=transforms
    )
  }
}

case class PivotFieldParams (calculate:Boolean, pivotFieldState:Option[PivotFieldsState])
