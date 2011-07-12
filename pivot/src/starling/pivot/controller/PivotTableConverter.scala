package starling.pivot.controller

import starling.pivot.model._
import starling.pivot._
import starling.quantity.{SpreadOrQuantity, Quantity, UOM}
import starling.utils.ImplicitConversions._
import collection.mutable.ListBuffer
import collection.Set
import collection.immutable.{Iterable, Map}
import starling.utils.{STable, SColumn}


object AxisNode {
  val Null = AxisNode(AxisValue.Null)

  def textAndAlignment(value:AxisValue, formatInfo:FormatInfo, extraFormatInfo:ExtraFormatInfo) = {

    def tableCell(value0:Any) = {
      val formatter = formatInfo.fieldToFormatter(value.field)
      value0 match {
        case UndefinedValue => TableCell.Undefined
        case other => formatter.format(other, extraFormatInfo)
      }
    }

    value.value match {
      case t@TotalAxisValueType => (t.value.toString, LeftTextPosition)
      case n@NullAxisValueType => (n.value, LeftTextPosition)
      case m:MeasureAxisValueType => (m.value, LeftTextPosition)
      case v:ValueAxisValueType => {
        val tc = tableCell(v.value)
        (tc.text, (if (tc.textPosition == CenterTextPosition) LeftTextPosition else tc.textPosition))
      }
    }
  }
}

case class ChildKey(value:Any, isMeasure:Boolean, field:Field, position:Int) {
  def toTotal = copy(value = TotalValue)
}

object ServerAxisNode {
  val Null = ServerAxisNode(AxisValue.Null)
}
case class ServerAxisNode(axisValue:AxisValue, children:Map[ChildKey,Map[AxisValue,ServerAxisNode]]=Map.empty) {
  val childValues:List[ServerAxisNode] = children.values.map(_.values).flatten.toList

  def add(values:List[AxisValue]):ServerAxisNode = {
    values match {
      case Nil => this
      case head :: tail => {
        children.get(head.keyValue) match {
          case Some(matches) => {
            val updatedMap = matches.get(head) match {
              case Some(matched) => matches.updated(head, matched.add(tail))
              case None => matches.updated(head, ServerAxisNode(head).add(tail))
            }
            copy(children = children.updated(head.keyValue, updatedMap))
          }
          case None => copy(children = children.updated(head.keyValue, Map(head -> ServerAxisNode(head).add(tail))))
        }
      }
    }
  }

  def totalChildren:Set[List[AxisValue]] = {
    (childValues.flatMap { child => {
      val head = if (child.axisValue.isMeasure) child.axisValue else child.axisValue.toTotal
      val childChildren = child.totalChildren
      if (childChildren.isEmpty) {
        Set(head :: Nil)
      } else {
        childChildren.map(l => head :: l)
      }
    }}).toSet
  }

  def withTotals:ServerAxisNode = {
    val childTotals = children.mapValues(_.mapValues(_.withTotals))
    val x = copy(children = childTotals)
    val childrenAsTotals:Set[List[AxisValue]] = totalChildren
    childrenAsTotals.foldLeft(x)((x1,t) => {
      x1.add(t)
    })
  }

  def toGUIAxisNode(fieldDetailsLookup:Map[Field, FieldDetails]):AxisNode = {
    val sortedChildren = children.toList.sortWith{ case((childKeyA,mapA),(childKeyB, mapB)) => {
      if (childKeyA.position == childKeyB.position) {
        val comparator = fieldDetailsLookup(childKeyA.field).comparator
        def index(value:Any) = {
          value match {
            case TotalValue=> 0
            case UndefinedValue => 1
            case FilterWithOtherTransform.OtherValue => 3
            case ptp:PivotTreePath if ptp.isOther => 3
            case _ => 2
          }
        }
        val thisIndex = index(childKeyA.value)
        val otherIndex = index(childKeyB.value)
        if (thisIndex == otherIndex) {
          if (thisIndex == 2) {
            comparator.lt(childKeyA.value, childKeyB.value)
          } else {
            false
          }
        } else {
          thisIndex < otherIndex
        }
      } else {
        childKeyA.position < childKeyB.position
      }
    } }

    val flattenedChildren = sortedChildren.flatMap{ case (_, map) => {
      map.values.toList.sortBy(_.axisValue.state)
    } }
    
    AxisNode(axisValue, flattenedChildren.map(_.toGUIAxisNode(fieldDetailsLookup)))
  }

}

case class AxisNode(axisValue:AxisValue, children:List[AxisNode]=Nil) {
  def purge(remove:Set[List[AxisValue]], parent:List[AxisValue] = Nil):Option[AxisNode] = {
    val pathToMe = axisValue :: parent
    if (children.isEmpty) {
      if (remove.contains(pathToMe.reverse)) None else Some(this)
    } else {
      val purgedChildren = children.flatMap{ childNode => { childNode.purge(remove, pathToMe) }}
      if (purgedChildren.isEmpty) {
        None
      } else {
        Some(AxisNode(axisValue, purgedChildren))
      }
    }
  }

  def flatten(path:List[AxisValue], subTotals:Boolean, recursiveCollapsed:Boolean, collapsedState:CollapsedState,
              disabledSubTotals:List[Field], formatInfo:FormatInfo, extraFormatInfo:ExtraFormatInfo):List[List[AxisCell]] = {
    val pathToHere = axisValue :: path
    val collapsed = recursiveCollapsed || collapsedState.collapsed(pathToHere.reverse.tail) || (subTotals && axisValue.isTotal)
    val filteredChildren = (if (collapsed) {
      children.filter { c=> c.axisValue.isTotal || c.axisValue.isMeasure }
    } else if (!subTotals || disabledSubTotals.contains(axisValue.field)) {
      children.filterNot { c=> c.axisValue.isTotal }
    } else {
      children
    })
    val childCells:List[List[AxisCell]] = filteredChildren.flatMap{ child=>child.flatten(pathToHere, subTotals,
      collapsed, collapsedState, disabledSubTotals, formatInfo, extraFormatInfo)}
    val f = childCells match {
      case Nil => {
        val (text, alignment) = AxisNode.textAndAlignment(axisValue, formatInfo, extraFormatInfo)
        List(List(AxisCell(axisValue, Some(1), text, None, false, NotTotal, 0, alignment)))
      }
      case head :: tail => {
        val totalSpan = childCells.flatMap(_.head.span).sum
        val nonMeasureFilteredChildren = filteredChildren.filterNot(_.axisValue.isMeasure)
        val collapsible = if (axisValue.isTotal || recursiveCollapsed || (!collapsed && (nonMeasureFilteredChildren.size <= 1))) None else Some(collapsed)
        val fixedHead = head match {
          case first :: rest if (axisValue.isTotal && first.value.isTotal)  => {
            first.copy(hidden=true) :: rest
          }
          case _ => head
        }
        val (text, alignment) = AxisNode.textAndAlignment(axisValue, formatInfo, extraFormatInfo)
        List(AxisCell(axisValue, Some(totalSpan), text, collapsible, false, NotTotal, 0, alignment) :: fixedHead) ::: tail.zipWithIndex.map{ case(c,index) => {
          AxisCell(axisValue, None, text, None, true, NotTotal, index+1, alignment) :: c
        }}
      }
    }
    if (axisValue.isTotal) {
      f.map(_.map(_.copy(totalState=SubTotal)))
    } else if (axisValue.isOtherValue) {
      f.map(_.map(ac => {
        if (ac.isTotalValue) ac else ac.copy(totalState=OtherValueTotal)
      }))
    } else {
      f
    }
  }
}

object AxisNodeBuilder {
  def flatten(node:AxisNode, grandTotals:Boolean, subTotals:Boolean, collapsedState:CollapsedState,
              disabledSubTotals:List[Field], formatInfo:FormatInfo, extraFormatInfo:ExtraFormatInfo,
              grandTotalsOnEachSide:Boolean):List[List[AxisCell]] = {
    val disabledSubTotalsToUse = Field.NullField :: Field.RootField :: disabledSubTotals
    val cells = node.flatten(List(), subTotals, false, collapsedState, disabledSubTotalsToUse, formatInfo, extraFormatInfo)
    val width = cells(0).length
    val grandTotalRows = if (grandTotals) {
      val rows = node.flatten(List(), false, true, collapsedState, disabledSubTotalsToUse, formatInfo, extraFormatInfo)
      rows.map(l => l.map(_.copy(totalState=Total)).padTo(width, AxisCell.NullTotal))
    } else {
      List()
    }

    val frontCells = if (grandTotalsOnEachSide) {
      grandTotalRows
    } else {
      List()
    }
    val cellsWithNull = if (cells.length > 1) {
      frontCells ::: cells ::: grandTotalRows
    } else {
      cells
    }
    cellsWithNull.map(r=>r.tail)
  }
}

/**
 * Supplies data for the pivot table view converted using totals and expand/collapse state.
 */
case class PivotTableConverter(otherLayoutInfo:OtherLayoutInfo = OtherLayoutInfo(), table:PivotTable,
                               extraFormatInfo:ExtraFormatInfo=PivotFormatter.DefaultExtraFormatInfo,
                               fieldState:PivotFieldsState=PivotFieldsState()) {
  val totals = otherLayoutInfo.totals
  val collapsedRowState = otherLayoutInfo.rowCollapsedState
  val collapsedColState = otherLayoutInfo.columnCollapsedState

  def allTableCells(extractUOMs:Boolean = true) = {
    val grid = createGrid(extractUOMs)
    (grid.rowData, grid.colData, grid.mainData)
  }

  def allTableCellsAndUOMs = {
    val grid = createGrid(true)
    (grid.rowData, grid.colData, grid.mainData, grid.colUOMS)
  }

  def createGrid(extractUOMs:Boolean = true, addExtraColumnRow:Boolean = true):PivotGrid ={
    val aggregatedMainBucket = table.aggregatedMainBucket
    val zeroFields = table.zeroFields
    val rowsToRemove:Set[List[AxisValue]] = if (otherLayoutInfo.removeZeros && (fieldState.columns.allFields.toSet & zeroFields).nonEmpty) {
      val rows:Set[List[AxisValue]] = aggregatedMainBucket.groupBy{case ((r,c),v) => r}.keySet
      rows.flatMap(row => {
        val onlyZeroFieldColumnsMap = aggregatedMainBucket.filter{case ((r,c),_) => {
          (r == row) && (c.find(_.isMeasure) match {
            case None => false
            case Some(an) => zeroFields.contains(an.field)
          })
        }}
        if (onlyZeroFieldColumnsMap.forall{case (_,v) => v match {
          case q:Quantity => q.isAlmostZero
          case pq:PivotQuantity => pq.isAlmostZero
          case _ => false
        }}) Some(row) else None
      })
    } else {
      Set[List[AxisValue]]()
    }

    val rowData = AxisNodeBuilder.flatten(table.rowNode.purge(rowsToRemove).getOrElse(AxisNode.Null), totals.rowGrandTotal,
      totals.rowSubTotals, collapsedRowState, otherLayoutInfo.disabledSubTotals, table.formatInfo, extraFormatInfo, true)

    def insertNullWhenNoRowValues(grid:List[List[AxisCell]], nullCount:Int) = {
      grid.map{ r=> {
        if (r.isEmpty) List.fill(math.max(1, nullCount))(AxisCell.Null) else r
      }}
    }
    val rowDataWithNullsAdded = {
      val r  = insertNullWhenNoRowValues(rowData, table.rowFieldHeadingCount.sum)
      table.editableInfo match {
        case None => r
        case Some(info) => {
          val keyFields = info.editableKeyFields.keySet
          val editableColIndices = table.rowFields.zipWithIndex.filter{case (f,index) => keyFields.contains(f)}.map(_._2).toSet
          r.map(cols => {
            cols.zipWithIndex.map{case (cell,index) => if (cell.shown && cell.notTotalValue && editableColIndices.contains(index)) {
              cell.copy(editable = true)
            } else {
              cell
            }}
          })
        }
      }
    }
    val extraDisabledSubTotals:List[Field] = {
      def findFieldsWithNullChildren(an0:AxisNode):List[Field] = {
        if (an0.children.isEmpty) {
          Nil
        } else if (an0.children.exists(_.axisValue.field == Field.NullField)) {
          List(an0.axisValue.field)
        } else {
          an0.children.flatMap(findFieldsWithNullChildren(_))
        }
      }
      table.columnAxis.flatMap(an => findFieldsWithNullChildren(an)).distinct
    }
    val cdX = AxisNodeBuilder.flatten(table.columnNode, totals.columnGrandTotal, totals.columnSubTotals, collapsedColState,
       extraDisabledSubTotals ::: otherLayoutInfo.disabledSubTotals, table.formatInfo, extraFormatInfo, false)
    
    val cd = {
      val r = insertNullWhenNoRowValues(cdX, 1)
      // I always want there to be at least 2 rows in the column header table area so that the row field drop area is visible.
      if (addExtraColumnRow && r(0).length < 2) {
        r.map(l => {
          List.fill(r(0).size)(AxisCell.Filler) ::: l
        })
      } else {
        r
      }
    }

    val colData = new Array[Array[AxisCell]]( if (cd.size==0) 0 else cd(0).size )
    for (i <- (0 until colData.size)) {
      colData(i) = new Array[AxisCell](cd.size)
    }
    cd.zipWithIndex.foreach { case(row, r) => {
      row.zipWithIndex.foreach { case (value, c) => {
        colData(c)(r) = value
      }}
    }}

    // We need to check dimensions here as if the table is too big we run out of memory.
    if (rowDataWithNullsAdded.length * colData(0).length > 1000000) {
      val fakeRowData = Array(Array(AxisCell.Null))
      val fakeColData = Array(Array(AxisCell.Null))
      val fakeMainData = Array(Array(TableCell("Table too big, rearrange fields. " +
              "The report ran but the table to display the result is too big, please rearrange fields or call a developer")))
      PivotGrid(fakeRowData, fakeColData, fakeMainData)
    } else {
      // Note below that we are using rowData rather than rowDataWithNullsAdded. This is because the rowData matches the aggregatedMainBucket.
      val (mainData, columnUOMs) = nMainTableCells(rowData, cdX, extractUOMs)

      if (extractUOMs) {
        // Extract the UOM label as far towards the top of the column header table as possible.
        val startRow = colData.indexWhere(_(0) != AxisCell.Filler)
        if (startRow != -1) {

          def getSpans(row:Array[AxisCell]):List[(Int,Int)] = {
            val spans = new ListBuffer[(Int,Int)]()
            var currentCol = 0
            while (currentCol < row.length) {
              row(currentCol).span match {
                case None => {
                  spans += ((currentCol,currentCol))
                  currentCol += 1
                }
                case Some(c) => {
                  spans += ((currentCol,currentCol+c-1))
                  currentCol += c
                }
              }
            }
            spans.toList
          }

          var columnsNotHandled = (0 until columnUOMs.length).toSet.filter(n => columnUOMs(n).asString.length() > 0)
          var currentRow = startRow
          while (columnsNotHandled.nonEmpty && (currentRow < colData.length)) {
            val spans = getSpans(colData(currentRow)).filter{case (start, end) => columnsNotHandled.contains(start)}
            spans.foreach{case (start, end) => {
              if ((start to end).map(c => columnUOMs(c)).distinct.size == 1) {
                val current = colData(currentRow)(start)
                val uom = columnUOMs(start).asString
                if (uom.length > 0) {
                  colData(currentRow)(start) = current.changeLabel(current.text + " (" + uom + ")")
                }
                columnsNotHandled --= (start to end).toSet
              }
            }}
            currentRow += 1
          }
        }
      }
      PivotGrid(rowDataWithNullsAdded.map(_.toArray).toArray, colData, mainData, columnUOMs)
    }
  }

  private def nMainTableCells(flattenedRowValues:List[List[AxisCell]], flattenedColValues:List[List[AxisCell]], extractUOMs:Boolean = true) = {
    val aggregatedMainBucket = table.aggregatedMainBucket

    //create the main table looping through the flattened rows and columns and looking up the sums in mainTableBucket
    val allUnits = Array.fill(scala.math.max(1, flattenedColValues.size))(Set[UOM]())
    val data: Array[Array[TableCell]] =
      (for ((rowValues, rowIndex) <- flattenedRowValues.zipWithIndex) yield {
        val rowSubTotal = rowValues.exists(_.totalState == SubTotal)
        val rowTotal = rowValues.exists(_.totalState == Total)
        val rowOtherValue = rowValues.exists(_.totalState == OtherValueTotal)
        (for ((columnValues, columnIndex) <- flattenedColValues.zipWithIndex) yield {
          val key = (rowValues.map(_.value).toList, columnValues.map(_.value).toList)

          def appendUOM(value:Any) {
            value match {
              case q:PivotQuantity => allUnits(columnIndex) = allUnits(columnIndex) ++ q.uoms
              case q:Quantity => allUnits(columnIndex) = allUnits(columnIndex) + q.uom
              case SpreadOrQuantity(Left(q)) =>  allUnits(columnIndex) = allUnits(columnIndex) + q.uom
              case SpreadOrQuantity(Right(sq)) =>  allUnits(columnIndex) = allUnits(columnIndex) + sq.uom
              case _ =>
            }
          }

          val tableCell = aggregatedMainBucket.get(key) match {
            case Some(measureCell) => {
              measureCell.value match {
                case Some(s:Set[_]) => s.foreach(appendUOM)
                case Some(v) => appendUOM(v)
                case None =>
              }
              columnValues.find(ac => ac.value.isMeasure) match {
                case None => {
                  // This is probably a "fake" message cell.
                  measureCell.value match {
                    case Some(v) => TableCell(v)
                    case _ => TableCell.Null
                  }
                }
                case Some(measureAxisCell) => {
                  val tc = measureCell.value match {
                    case None => TableCell.Null
                    case Some(UndefinedValue) => TableCell.Undefined
                    case Some(other) => table.formatInfo.fieldToFormatter(measureAxisCell.value.field).format(other, extraFormatInfo)
                  }
                  tc.copy(state = measureCell.cellType, edits = measureCell.edits)
                }
              }
            }
            case None => TableCell.Null
          }

          val columnSubTotal = columnValues.exists(_.totalState == SubTotal)
          val columnTotal = columnValues.exists(_.totalState == Total)
          val columnOtherValue = columnValues.exists(_.totalState == OtherValueTotal)

          val editable = table.editableInfo match {
            case None => false
            case Some(editableInfo) => columnValues.map(_.value.field).toSet.intersect(editableInfo.editableMeasures.keySet).nonEmpty &&
                    !(rowSubTotal || rowTotal || rowOtherValue || columnSubTotal || columnTotal || columnOtherValue)
          }

          if ((rowTotal && columnSubTotal) || (columnTotal && rowSubTotal) || (rowTotal && columnTotal) || (rowSubTotal && columnSubTotal)) {
            tableCell.copy(totalState=SubtotalTotal, editable = editable)
          } else if (rowTotal || columnTotal) {
            tableCell.copy(totalState=Total, editable = editable)
          } else if (rowSubTotal || columnSubTotal) {
            tableCell.copy(totalState=SubTotal, editable = editable)
          } else if (rowOtherValue || columnOtherValue) {
            tableCell.copy(totalState=OtherValueTotal, editable = editable)
          } else {
            tableCell.copy(editable = editable)
          }
        }).toArray
      }).toArray


    if (extractUOMs) {
      // If a column only has one uom, set that uom as the column header.
      for ((row, rowIndex) <- data.zipWithIndex) {
        for ((value,colIndex) <- data(rowIndex).zipWithIndex) {
          val uomSet = allUnits(colIndex)
          if (uomSet.size == 1) {
            val newText = value.value match {
              case q:PivotQuantity => PivotFormatter.formatPivotQuantity(q, extraFormatInfo, false)
              case q:Quantity => q.value.format(extraFormatInfo.decimalPlaces.format(q.uom))
              case _ => value.text
            }
            data(rowIndex)(colIndex) = value.copy(text = newText)
          }
        }
      }
    }

    val columnUOMs = allUnits.map(uomSet => {
      if (uomSet.size == 1) {
        uomSet.iterator.next
      } else {
        UOM.NULL
      }
    })


    (data, columnUOMs)
  }

  def toSTable(name:String) = {
    val rowAxis = table.rowAxis
    val (rowHeaderCells, columnHeaderCells, mainTableCells) = allTableCells(false)
    val rowHeader= if (rowHeaderCells.isEmpty) List() else rowHeaderCells(0).map { cv => SColumn(cv.value.field.name) }.toList
//    val columnHeader = columnHeaderCells(0).map { cv => SColumn(cv.value.field.name) }.toList
    val columnHeader = columnHeaderCells(1).map { cv => SColumn(cv.label) }.toList

    //Does not work if the column area contains non-measures
    STable(
      name,
      rowHeader ::: columnHeader,
      (for ((row, rowIndex) <- rowHeaderCells.zipWithIndex) yield {
        val measures = mainTableCells(rowIndex)
        row.map{cv=>cv.value.value.value }.toList ::: measures.map { tc => tc }.toList
      }).toList
    )
  }
}