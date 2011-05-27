package starling.pivot.controller

import starling.pivot.model._
import starling.pivot._
import starling.utils.{STable, SColumn}
import starling.quantity.{SpreadOrQuantity, Quantity, UOM}
import starling.utils.ImplicitConversions._
import collection.mutable.ListBuffer

object AxisNode {
  def textAndAlignment(value:AxisValue, formatInfo:FormatInfo, extraFormatInfo:ExtraFormatInfo) = {
    value.value match {
      case t@TotalAxisValueType => (t.value, LeftTextPosition)
      case n@NullAxisValueType => (n.value, LeftTextPosition)
      case m:MeasureAxisValueType => (m.value, LeftTextPosition)
      case v:ValueAxisValueType => {
        val formatter = formatInfo.fieldToFormatter(value.field)
        val tc = v.value match {
          case UndefinedValue => TableCell.Undefined
          case other => formatter.format(other, extraFormatInfo)
        }
        (tc.text, (if (tc.textPosition == CenterTextPosition) LeftTextPosition else tc.textPosition))
      }
    }
  }
}

case class AxisNode(axisValue:AxisValue, children:List[AxisNode]) {
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
  def createNodes(grid:Array[Array[AxisValue]], fromRow:Int, toRow:Int, fromCol:Int):List[AxisNode] = {
    if (toRow <= grid.size && grid.size > 0 && fromCol < grid(0).size) {
      val buffer = new scala.collection.mutable.ArrayBuffer[AxisNode]()
      var lastRow = fromRow

      var lastValue = grid(fromRow)(fromCol)
      (fromRow until toRow).foreach { row => {
        val value = grid(row)(fromCol)
        if (value != lastValue) {
          val children = createNodes(grid, lastRow, row, fromCol+1)
          buffer.append( AxisNode(lastValue, children) )
          lastValue = value
          lastRow = row
        }
      }}
      buffer.append( AxisNode(lastValue, createNodes(grid,  lastRow, toRow, fromCol+1)) )
      buffer.toList
    } else {
      List()
    }
  }

  def flatten(nodes:List[AxisNode], grandTotals:Boolean, subTotals:Boolean, collapsedState:CollapsedState,
              disabledSubTotals:List[Field], formatInfo:FormatInfo, extraFormatInfo:ExtraFormatInfo):List[List[AxisCell]] = {
    val fakeField = Field("N")
    val disabledSubTotalsToUse = Field.NullField :: fakeField :: disabledSubTotals

    println("")
    println(disabledSubTotalsToUse)
    println("")


    val fakeNode = AxisNode(AxisValue(fakeField, NullAxisValueType, 0), nodes)
    val grandTotalRows = if (grandTotals) {
      val rows = fakeNode.flatten(List(), false, true, collapsedState, disabledSubTotalsToUse, formatInfo, extraFormatInfo)
      rows.map(_.map(_.copy(totalState=Total)))
    } else {
      List()
    }
    val cellsWithNull = fakeNode.flatten(List(), subTotals, false, collapsedState, disabledSubTotalsToUse, formatInfo, extraFormatInfo) ::: grandTotalRows
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
    val rowDataX = AxisNodeBuilder.flatten(table.rowAxis, totals.rowGrandTotal, totals.rowSubTotals, collapsedRowState,
      otherLayoutInfo.disabledSubTotals, table.formatInfo, extraFormatInfo)
    def insertNull(grid:List[List[AxisCell]], nullCount:Int) = {
      grid.map{ r=> {
        if (r.isEmpty) List.fill(math.max(1, nullCount))(AxisCell.Null) else r
      }}
    }
    val rowData = {
      val r  = insertNull(rowDataX, table.rowFieldHeadingCount.sum)
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
    // We never want to show sub totals for top level measure fields.
    val extraDisabledSubTotals = {
      val topLevelMeasureFields = table.columnAxis.flatMap(an => {
        if (an.axisValue.isMeasure) {
          Some(an.axisValue.field)
        } else {
          None
        }
      })
      val measureFieldsBeneathFields = otherLayoutInfo.disabledSubTotals.flatMap(f => fieldState.columns.measureFieldsDirectlyBeneath(f))
      topLevelMeasureFields ::: measureFieldsBeneathFields
    }
    val cdX = AxisNodeBuilder.flatten(table.columnAxis, totals.columnGrandTotal, totals.columnSubTotals, collapsedColState,
       extraDisabledSubTotals ::: otherLayoutInfo.disabledSubTotals, table.formatInfo, extraFormatInfo)
    
    val cd = {
      val r = insertNull(cdX, 1)
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
    if (rowData.length * colData(0).length > 1000000) {
      val fakeRowData = Array(Array(AxisCell.Null))
      val fakeColData = Array(Array(AxisCell.Null))
      val fakeMainData = Array(Array(TableCell("Table too big, rearrange fields. " +
              "The report ran but the table to display the result is too big, please rearrange fields or call a developer")))
      PivotGrid(fakeRowData, fakeColData, fakeMainData)
    } else {
      val (mainData, columnUOMs) = nMainTableCells(rowDataX, cdX, extractUOMs)

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
          while (columnsNotHandled.nonEmpty) {
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
      PivotGrid(rowData.map(_.toArray).toArray, colData, mainData, columnUOMs)
    }
  }

  private def nMainTableCells(flattenedRowValues:List[List[AxisCell]], flattenedColValues:List[List[AxisCell]], extractUOMs:Boolean = true) = {
    val numCols = flattenedColValues.size
    val numRows = flattenedRowValues.size
    val numRowHeaderColumns = if (flattenedRowValues.size==0) 0 else flattenedRowValues(0).size
    
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
            case Some(value) => {
              value match {
                case s:Set[_] => s.foreach(appendUOM)
                case v => appendUOM(v)
              }
              columnValues.find(ac => ac.value.isMeasure) match {
                case None => TableCell(value)
                case Some(measureAxisCell) => {
                  value match {
                    case UndefinedValue => TableCell.Undefined
                    case other => table.formatInfo.fieldToFormatter(measureAxisCell.value.field).format(other, extraFormatInfo)
                  }
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