package starling.pivot.controller

import starling.pivot.model._
import starling.pivot._
import starling.utils.ImplicitConversions._
import starling.utils.{GridConverter, Utils}
import collection.immutable.List._

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


/**
 * this is the Pivot Table as returned by the controller
 * and used by the view. Use this *and not* the PivotTableModel
 * This should contain all the info required for rendering the
 * table in whatever fashion
 */
case class PivotTable(rowFields:List[Field], rowFieldHeadingCount:Array[Int], rowAxis:List[AxisNode],
                      columnAxis:List[AxisNode], possibleValues:Map[Field,TreePivotFilter], treeDetails:TreeDetails,
                      editableInfo:Option[EditableInfo], formatInfo:FormatInfo,
                      aggregatedMainBucket:Map[(List[AxisValue],List[AxisValue]),Any] = Map()) {

  def asCSV:String = convertUsing(Utils.csvConverter)
  def convertUsing(converter: GridConverter) = converter.convert(toFlatRows(Totals.Null))
  def toFlatRows(totals:Totals): List[List[Any]] = toFlatRows(totals, (tc:TableCell)=>tc.text, (ac:AxisCell)=>ac.text)

  def cell(measure: AnyRef, filters: (Field, AnyRef)*): Any = {
    def filter(name: String, value: AnyRef, index: Int)(input: Map[(List[AxisValue], List[AxisValue]), Any]) =
      input.filter { case ((rows, cols_), tableCell) => {
        val axisValue = rows(index)
        val field = axisValue.field
        val tc = axisValue.value.value match {
          case UndefinedValue => TableCell.Undefined
          case v => formatInfo.fieldToFormatter(field).format(v, PivotFormatter.DefaultExtraFormatInfo)
        }
        tc.text.equalsIgnoreCase(value.toString()) }}
        .deny(_.isEmpty, "No %s found for %s %s" % (measure, name, value))

    val filterFns = filters.zipWithIndex.map{ case ((field, value), index) => filter(field.name, value, index) _ }

    val matches = aggregatedMainBucket.applyAll(filterFns : _*)

    matches.iterator.next._2
  }

  private def toFlatRows(totals:Totals, tableCell:(TableCell)=>Any, axisCell:(AxisCell)=>Any):List[List[Any]] = {
    val pivotTableConverter = PivotTableConverter(OtherLayoutInfo(totals = totals), this)

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
      rowBuffer ++= row.map(acv => axisCell(acv))
      rowsBuffer += rowBuffer.toList
    }

    for ((row, data) <- rowHeaderCells zip mainTableCells) {
      val rowBuffer = new scala.collection.mutable.ArrayBuffer[Any]()
      rowBuffer ++= row.map(ac => axisCell(ac))
      rowBuffer ++= data.map(tc => tableCell(tc))
      rowsBuffer += rowBuffer.toList
    }

    rowsBuffer.toList
  }
}

object PivotTable {
  def singleCellPivotTable(text:String) = {
    val bucket = Map( (List(AxisValue.Null), List(AxisValue.Null)) -> TableCell(text))
    PivotTable(List(), Array(), List(AxisNode(AxisValue.Null, List())), List(AxisNode(AxisValue.Null, List())), Map(), TreeDetails(Map(), Map()), None, FormatInfo.Blank, bucket)
  }
}