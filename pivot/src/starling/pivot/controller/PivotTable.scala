package starling.pivot.controller

import starling.pivot.model._
import starling.pivot._
import starling.utils.ImplicitConversions._
import starling.utils.{GridConverter, Utils}
import collection.immutable.List._
import java.lang.Boolean

case class TreePivotFilter(root:TreePivotFilterNode)
case class TreePivotFilterNode(value:Any, label:String, children:List[TreePivotFilterNode]) {
  def this(value:Any) = this(value, value.toString, List())
  def this(value:Any, children:List[TreePivotFilterNode]) = this(value, value.toString, children)
}

object TreePivotFilterNode {
  def mergeForests(forest: List[TreePivotFilterNode]) = mergeForestsBy(forest, _.label)
  def mergeForestsValues(forest: List[TreePivotFilterNode]) = mergeForestsBy(forest, _.value)

  private def mergeForestsBy[A](forest: List[TreePivotFilterNode], prop: TreePivotFilterNode => A): List[TreePivotFilterNode] = {
    def merge(tree: TreePivotFilterNode, forest: List[TreePivotFilterNode]): List[TreePivotFilterNode] = forest match {
      case Nil => List(tree)
      case first :: rest if (prop(tree) == prop(first)) =>
        merge(tree.copy(children = mergeForestsBy(first.children ++ tree.children, prop)), rest)
      case first :: rest => first :: merge(tree, rest)
    }

    forest.foldLeft(List[TreePivotFilterNode]())((subForest,tree) => merge(tree, subForest))
  }
}

case class PivotTable(rowFields:List[Field], rowFieldHeadingCount:Array[Int], rowNode:AxisNode,
                      columnNode:AxisNode, possibleValues:Map[Field,TreePivotFilter], treeDetails:TreeDetails,
                      editableInfo:Option[EditableInfo], fieldInfo:FieldInfo,
                      aggregatedMainBucket:Map[(List[ChildKey],List[ChildKey]),MeasureCell] = Map(),
                      zeroFields:Set[Field]=Set()) {

  def rowAxis = rowNode.children
  def columnAxis = columnNode.children

  def asCSV:String = convertUsing(Utils.csvConverter)
  def convertUsing(converter: GridConverter, extraFormatInfo: ExtraFormatInfo = PivotFormatter.DefaultExtraFormatInfo) =
    converter.convert(toFlatRows(Totals.Null, extraFormatInfo, true))

  def cell(measure: AnyRef, filters: (Field, AnyRef)*): Any = {
    def filter(name: String, value: AnyRef, index: Int)(input: Map[(List[ChildKey], List[ChildKey]), MeasureCell]) =
      input.filter { case ((rows, cols_), tableCell) => {
        val childKey = rows(index)
        val field = childKey.field
        val tc = childKey.value match {
          case UndefinedValue => TableCell.Undefined
          case v => fieldInfo.fieldToFormatter(field).format(v, PivotFormatter.DefaultExtraFormatInfo)
        }
        tc.text.equalsIgnoreCase(value.toString()) }}
        .deny(_.isEmpty, "No %s found for %s %s" % (measure, name, value))

    val filterFns = filters.zipWithIndex.map{ case ((field, value), index) => filter(field.name, value, index) _ }

    val matches = aggregatedMainBucket.applyAll(filterFns : _*)

    matches.iterator.next._2.value.getOrElse(throw new Exception("No matches for " + filters))
  }

  def toFlatRows(totals: Totals, extraFormatInfo:ExtraFormatInfo = PivotFormatter.DefaultExtraFormatInfo,
                 trimBlank: Boolean = false, convertToText: Boolean = true):
    List[List[Any]] = {

    val pivotTableConverter = PivotTableConverter(OtherLayoutInfo(totals = totals), this, extraFormatInfo)

    val (rowHeaderCells, columnHeaderCells, mainTableCells) = pivotTableConverter.allTableCells()

    // Unlike the gui, the spread sheets don't want columns or rows spanned, they want the value repeated.
    for (j <- 0 until columnHeaderCells.length) {
      val row = columnHeaderCells(j)
      for (i <- 0 until row.length) {
        row(i).span.map { span =>
          for (delta <- (i + 1) until (i + span)) row(delta) = row(i).copy(span = None)
        }
      }
    }

    for (i <- 0 until rowHeaderCells.length) {
      val row = rowHeaderCells(i)
      for (j <- 0 until row.length) {
        row(j).span.map { span =>
          for (delta <- (i + 1) until (i + span)) rowHeaderCells(delta)(j) = row(j).copy(span = None)
        }
      }
    }

    val rowHeaders = if (rowHeaderCells.isEmpty) rowFields.map(_.name) else {
      rowHeaderCells(0).map(_.value.field.name).toList
    }

    val rowsBuffer = new scala.collection.mutable.ArrayBuffer[List[Any]]()

    for ((row,index) <- columnHeaderCells.zipWithIndex) {
      val rowBuffer = new scala.collection.mutable.ArrayBuffer[Any]()
      //Not: the column/data field names do not apear anywhere as in the general case they can not be mapped
      if (index != columnHeaderCells.size-1) { //blank cells in the to left area before the column headings
        rowBuffer ++= Array.fill(rowHeaders.size)("")
      } else {
        rowBuffer ++= rowHeaders //use the last row for the row field names
      }
      rowBuffer ++= (if (convertToText) row.map(_.text) else row)
      rowsBuffer += rowBuffer.toList
    }

    for ((row, data) <- rowHeaderCells zip mainTableCells) {
      val rowBuffer = new scala.collection.mutable.ArrayBuffer[Any]()
      rowBuffer ++= (if (convertToText) row.map(_.text) else row)
      rowBuffer ++= (if (convertToText) data.map(_.text) else data)
      rowsBuffer += rowBuffer.toList
    }

    if (trimBlank) {
      rowsBuffer.toList.filterNot(_.forall(_.toString.trim == ""))
    } else {
      rowsBuffer.toList
    }
  }
}

object PivotTable {
  def singleCellPivotTable(text:String) = {
    val bucket = Map( (List(AxisValue.Null.childKey), List(AxisValue.Null.childKey)) -> MeasureCell(Some(text), EditableCellState.Normal))
    PivotTable(List(), Array(), AxisNode.Null, AxisNode.Null, Map(), TreeDetails(Map(), Map()), None, FieldInfo.Blank, bucket)
  }
}