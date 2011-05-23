package starling.pivot.controller

import starling.pivot.model._
import starling.pivot._
import starling.utils.ImplicitConversions._


case class TreePivotFilter(root:TreePivotFilterNode)
case class TreePivotFilterNode(value:Any, label:String, children:List[TreePivotFilterNode]) {
  def this(value:Any) = this(value, value.toString, List())
  def this(value:Any, children:List[TreePivotFilterNode]) = this(value, value.toString, children)
}

object TreePivotFilterNode {
  def mergeForests(forest : List[TreePivotFilterNode]) : List[TreePivotFilterNode] = {
    def merge(tree : TreePivotFilterNode, forest : List[TreePivotFilterNode]) : List[TreePivotFilterNode] = {
      forest match {
        case Nil => List(tree)
        case first :: rest => {
          if (tree.label == first.label) {
            merge(tree.copy(children = mergeForests(first.children ++ tree.children)), rest)
          } else {
            first :: merge(tree, rest)
          }
        }
      }
    }

    forest.foldLeft(List[TreePivotFilterNode]())((subForest,tree) => merge(tree, subForest))
  }

  def mergeForestsValues(forest : List[TreePivotFilterNode]) : List[TreePivotFilterNode] = {
    def merge(tree : TreePivotFilterNode, forest : List[TreePivotFilterNode]) : List[TreePivotFilterNode] = {
      forest match {
        case Nil => List(tree)
        case first :: rest => {
          if (tree.value == first.value) {
            merge(tree.copy(children = mergeForests(first.children ++ tree.children)), rest)
          } else {
            first :: merge(tree, rest)
          }
        }
      }
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

  def asCSV:String = {
    val builder = new StringBuilder
    val rows = toFlatRows(Totals.Null)
    rows.foreach {
      row => builder.append( row.mkString(", ") + "\n")
    }
    builder.toString
  }

  def toFlatRows(totals:Totals):List[List[Any]] = {
    toFlatRows(totals, (tc:TableCell)=>tc.text, (ac:AxisCell)=>ac.text)
  }

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
    val rowsBuffer = new scala.collection.mutable.ArrayBuffer[List[Any]]()
    val pivotTableConverter = PivotTableConverter(OtherLayoutInfo(totals = totals), this)
    val (rowHeaderCells, columnHeaderCells, mainTableCells) = pivotTableConverter.allTableCells()
    for ((row,index) <- columnHeaderCells.zipWithIndex) {
      val rowBuffer = new scala.collection.mutable.ArrayBuffer[Any]()
      //Not: the column/data field names do not apear anywhere as in the general case they can not be mapped
      if (index != columnHeaderCells.size-1) { //blank cells in the to left area before the column headings
        rowBuffer ++= (0 until rowFields.size).map((_)=>"")
      } else {
        rowBuffer ++= rowFields.map(f=>f.name) //use the last row for the row field names
      }
      rowBuffer ++= row.map(acv=>axisCell(acv))
      rowsBuffer += rowBuffer.toList
    }
    for ((row,data) <- rowHeaderCells zip mainTableCells) {
      val rowBuffer = new scala.collection.mutable.ArrayBuffer[Any]()
      rowBuffer ++= row.map(ac=>axisCell(ac))
      rowBuffer ++= data.map(tc=> tableCell(tc))
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
