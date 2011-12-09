package starling.pivot.controller

import starling.pivot.model._
import starling.pivot._
import starling.quantity.{SpreadOrQuantity, Quantity, UOM}
import starling.utils.ImplicitConversions._
import collection.mutable.ListBuffer
import collection.Set
import collection.immutable.Map
import scalaz.Scalaz._
import starling.utils.{ImplicitConversions, STable, SColumn}


object AxisNode {
  val Null = AxisNode(AxisValue.Null, List(AxisNode(AxisValue.Null)))

  def textAndAlignment(value:AxisValue, formatInfo:FieldInfo, extraFormatInfo:ExtraFormatInfo) = {

    def tableCell(valueType:AxisValueType) = {
      val formatter = formatInfo.fieldToFormatter(value.field)
      valueType.value match {
        case UndefinedValueNew => TableCell.UndefinedNew
        case UndefinedValue if valueType.cellType == EditableCellState.Added => TableCell.UndefinedNew
        case UndefinedValue => TableCell.Undefined
        case other => formatter.format(other, extraFormatInfo)
      }
    }

    val (text, longText, alignment) = value.value match {
      case t@TotalAxisValueType => (t.value.toString, None, LeftTextPosition)
      case n@NullAxisValueType => (n.value, None, LeftTextPosition)
      case m:MeasureAxisValueType => (m.value, None, LeftTextPosition)
      case v:ValueAxisValueType => {
        val tc = tableCell(v)
        (tc.text, tc.longText, (if (tc.textPosition == CenterTextPosition) LeftTextPosition else tc.textPosition))
      }
      case t:CellTypeSpecifiedAxisValueType => {
        val tc = tableCell(t)
        (tc.text, tc.longText, (if (tc.textPosition == CenterTextPosition) LeftTextPosition else tc.textPosition))
      }
    }
    (text, longText.getOrElse(text), alignment)
  }
}

case class ChildKey(value:Any, isMeasure:Boolean, field:Field, position:Int) {
  def toTotal = copy(value = TotalValue)
}

object ServerAxisNode {
  val Null = ServerAxisNode(AxisValue.Null)
}
case class ServerAxisNode(axisValue:AxisValue, children:Map[ChildKey,Map[AxisValue,ServerAxisNode]]=Map.empty) {
  lazy val childValues:List[ServerAxisNode] = children.values.map(_.values).flatten.toList

  def add(values:List[AxisValue]):ServerAxisNode = {
    values match {
      case Nil => this
      case head :: tail => {
        children.get(head.childKey) match {
          case Some(matches) => {
            val updatedMap = matches.get(head) match {
              case Some(matched) => matches.updated(head, matched.add(tail))
              case None => matches.updated(head, ServerAxisNode(head).add(tail))
            }
            copy(children = children.updated(head.childKey, updatedMap))
          }
          case None => copy(children = children.updated(head.childKey, Map(head -> ServerAxisNode(head).add(tail))))
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

    val doubleUpAmmends:List[ServerAxisNode] = children.toList.flatMap{ case (childKey, map) => {
      map.toList.flatMap { case (av, node) => {
        av.value match {
          case v:ValueAxisValueType if map.size > 1 && v.cellType == EditableCellState.Edited => {
            v.pivotEdits.edits.keySet.toList match {
              case editFilter :: Nil => {
                val originalValue = CellTypeSpecifiedAxisValueType(EditableCellState.Tainted, v.originalValue.get, v.pivotEdits)
                val newValue = CellTypeSpecifiedAxisValueType(EditableCellState.Tainted, v.value, v.pivotEdits)
                List(
                  node.copy(axisValue=node.axisValue.copy(value = originalValue)),
                  node.copy(axisValue=node.axisValue.copy(value = newValue))
                )
              }
              case _ => throw new Exception("You always should have an edit " + v.pivotEdits.edits.keySet)
            }
          }
          case _ => List(node)
        }
      }}
    } }

    val r:List[ServerAxisNode] = doubleUpAmmends.groupBy(_.axisValue.childKey).map { case (childKey, nodes) => {
      val joinedValue = {
        val axisValues = nodes.map(_.axisValue)
        if (axisValues.size > 1) {
          val cellTypes = axisValues.map(_.value.cellType).toSet
          val cellType = cellTypes.toList match {
            case one :: Nil => one
            case _ => EditableCellState.Tainted
          }

          val editsToUse = axisValues.foldLeft(PivotEdits.Null)((edits,pv) => edits.addEdits(pv.pivotEdits))

          axisValues.head.copy(value=CellTypeSpecifiedAxisValueType(cellType, axisValues.head.value.value, editsToUse))
        } else {
          axisValues.head
        }
      }

      val childNodes: NestedMap[ChildKey, AxisValue, ServerAxisNode] = toNestedMap(nodes.flatMap(_.children.toList))

      ServerAxisNode(joinedValue, childNodes)
    } }.toList

    val sortedChildren:List[ServerAxisNode] = r.sortWith{ case(axisNode1,axisNode2) => {
      val axisValue1 = axisNode1.axisValue
      val axisValue2 = axisNode2.axisValue
      if (axisValue1.position == axisValue2.position) {
        val comparator = fieldDetailsLookup(axisValue1.field).comparator
        def index(axisValue:AxisValue) = {
          if (axisValue.childKey.value.isInstanceOf[NewRowValue]) {
            4
          } else {
            val value = axisValue.value.value
            value match {
              //case n:NewRowValue => 4
              case TotalValue=> 0
              case UndefinedValue | UndefinedValueNew => 1
              case FilterWithOtherTransform.OtherValue => 3
              case ptp:PivotTreePath if ptp.isOther => 3
              case _ => 2
            }
          }
        }

        (axisValue1.childKey.value, axisValue2.childKey.value) match {
          case (NewRowValue(r1), NewRowValue(r2)) => (r1 < r2)
          case _ => {
            val thisIndex = index(axisValue1)
            val otherIndex = index(axisValue2)
            if (thisIndex == otherIndex) {
              if (thisIndex == 2) {
                comparator.lt(axisValue1.value.originalOrValue, axisValue2.value.originalOrValue)
              } else {
                false
              }
            } else {
              thisIndex < otherIndex
            }
          }
        }
      } else {
        axisValue1.position < axisValue2.position
      }
    } }

    AxisNode(axisValue, sortedChildren.map(_.toGUIAxisNode(fieldDetailsLookup)))
  }
}

case class AxisNode(axisValue:AxisValue, children:List[AxisNode]=Nil) {
  def purge(remove:Set[List[ChildKey]], parent:List[ChildKey] = Nil):Option[AxisNode] = {
    val pathToMe = axisValue.isNull ? parent | axisValue.childKey :: parent
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

  def flatten(previous:Option[AxisNode], refresh:Boolean, path:List[AxisValue], subTotals:Boolean, recursiveCollapsed:Boolean, collapsedState:CollapsedState,
              disabledSubTotals:List[Field], formatInfo:FieldInfo, extraFormatInfo:ExtraFormatInfo):List[List[AxisCell]] = {
    val pathToHere = axisValue :: path
    val collapsed = recursiveCollapsed || collapsedState.collapsed(pathToHere.reverse.tail) || (subTotals && axisValue.isTotal)
    def createFilteredChildren(node:AxisNode):List[AxisNode] = {
      (if (collapsed) {
        node.children.filter { c=> c.axisValue.isTotal || c.axisValue.isMeasure }
      } else if (!subTotals || disabledSubTotals.contains(axisValue.field)) {
        node.children.filterNot { c=> c.axisValue.isTotal }
      } else {
        node.children
      })
    }
    val myFilteredChildren = createFilteredChildren(this)
    val previousFilteredChildren:Map[AxisValue,AxisNode] = previous match {
      case None => Map.empty
      case Some(p) => createFilteredChildren(p).map { node => node.axisValue -> node }.toMap
    }
    val filteredChildren:List[(AxisNode, Option[AxisNode])] = myFilteredChildren.map { child => (child, previousFilteredChildren.get(child.axisValue)) }
    val childCells:List[List[AxisCell]] = filteredChildren.flatMap{ case(child, previousChild) =>child.flatten(previousChild, refresh, pathToHere, subTotals,
      collapsed, collapsedState, disabledSubTotals, formatInfo, extraFormatInfo)}
    val changed = refresh && (Some(axisValue) != previous.map(_.axisValue))
    val childCellsWithoutTotals:List[List[AxisCell]] = childCells match {
      case Nil => {
        val (text, longText, alignment) = AxisNode.textAndAlignment(axisValue, formatInfo, extraFormatInfo)
        val ac = AxisCell(axisValue, Some(1), text, longText, None, false, NotTotal, 0, alignment, changed = changed)
        List(List(ac))
      }
      case head :: tail => {
        val totalSpan = childCells.flatMap(_.head.span).sum
        val nonMeasureFilteredChildren = filteredChildren.filterNot(_._1.axisValue.isMeasure)
        val collapsible = if (axisValue.isTotal || recursiveCollapsed || (!collapsed && (nonMeasureFilteredChildren.size <= 1))) None else Some(collapsed)
        val fixedHead = head match {
          case first :: rest if (axisValue.isTotal && first.value.isTotal)  => {
            first.copy(hidden=true) :: rest
          }
          case _ => head
        }
        val (text, longText, alignment) = AxisNode.textAndAlignment(axisValue, formatInfo, extraFormatInfo)
        val ac = AxisCell(axisValue, Some(totalSpan), text, longText, collapsible, false, NotTotal, 0, alignment, changed = changed)
        List(ac :: fixedHead) ::: tail.zipWithIndex.map{ case(c,index) => {
          val ac0 = AxisCell(axisValue, None, text, longText, None, true, NotTotal, index + 1, alignment, changed = changed)
          ac0 :: c
        }}
      }
    }
    if (axisValue.isTotal) {
      childCellsWithoutTotals.map(_.map(_.copy(totalState=SubTotal)))
    } else if (axisValue.isOtherValue) {
      childCellsWithoutTotals.map(_.map(ac => {
        if (ac.isTotalValue) ac else ac.copy(totalState=OtherValueTotal)
      }))
    } else {
      childCellsWithoutTotals
    }
  }
}

object AxisNodeBuilder {
  def flatten(node:AxisNode, grandTotals:Boolean, subTotals:Boolean, collapsedState:CollapsedState,
              disabledSubTotals:List[Field], formatInfo:FieldInfo, extraFormatInfo:ExtraFormatInfo,
              grandTotalsOnEachSide:Boolean, previousNode:Option[AxisNode]=None):List[List[AxisCell]] = {
    val disabledSubTotalsToUse = Field.NullField :: Field.RootField :: disabledSubTotals
    val cells = node.flatten(previousNode, previousNode.isDefined, List(), subTotals, false, collapsedState, disabledSubTotalsToUse, formatInfo, extraFormatInfo)
    val width = cells(0).length
    val grandTotalRows = if (grandTotals) {
      val rows = node.flatten(previousNode, previousNode.isDefined, List(), false, true, collapsedState, disabledSubTotalsToUse, formatInfo, extraFormatInfo)
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

case class CellUpdateInfo(row:Int, column:Int, matches:Boolean, currentFraction:Float)

/**
 * Supplies data for the pivot table view converted using totals and expand/collapse state.
 */
case class PivotTableConverter(otherLayoutInfo:OtherLayoutInfo = OtherLayoutInfo.Blank, table:PivotTable,
                               extraFormatInfo:ExtraFormatInfo=PivotFormatter.DefaultExtraFormatInfo,
                               fieldState:PivotFieldsState=PivotFieldsState(), previousPageData:Option[PivotTable]=None) {
  val totals = otherLayoutInfo.totals
  val collapsedRowState = otherLayoutInfo.rowCollapsedState
  val collapsedColState = otherLayoutInfo.columnCollapsedState
  private val previousAggregatedMainBucket = previousPageData.map(_.aggregatedMainBucket)

  def allTableCells(extractUOMs:Boolean = true) = {
    val grid = createGrid(extractUOMs)
    (grid.rowData, grid.colData, grid.mainData)
  }

  def allTableCellsAndUOMs = {
    val grid = createGrid(true)
    (grid.rowData, grid.colData, grid.mainData, grid.colUOMS, grid.mainTableUpdateInfo, grid.rowUpdateInfo, grid.columnUpdateInfo)
  }

  def createGrid(extractUOMs:Boolean = true, addExtraColumnRow:Boolean = true):PivotGrid ={
    val aggregatedMainBucket = table.aggregatedMainBucket
    val zeroFields = table.zeroFields
    val rowsToRemove:Set[List[ChildKey]] = (if (otherLayoutInfo.removeZeros && (fieldState.columns.allFields.toSet & zeroFields).nonEmpty) {
      val rows:Set[List[ChildKey]] = aggregatedMainBucket.groupBy{case ((r,c),v) => r}.keySet
      rows.flatMap(row => {
        val onlyZeroFieldColumnsMap = aggregatedMainBucket.filter{case ((r,c),_) => {
          (r == row) && (c.find(_.isMeasure) match {
            case None => false
            case Some(an) => zeroFields.contains(an.field)
          })
        }}
        onlyZeroFieldColumnsMap.forallValues(_.isAlmoseZero) option (row)
      })
    } else {
      Set[List[ChildKey]]()
    })

    val rowData = AxisNodeBuilder.flatten(table.rowNode.purge(rowsToRemove).getOrElse(AxisNode.Null), totals.rowGrandTotal,
      totals.rowSubTotals, collapsedRowState, otherLayoutInfo.disabledSubTotals, table.fieldInfo, extraFormatInfo, true, previousPageData.map(_.rowNode))

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
          val keyFields = info.keyFields
          val editableColIndices = table.rowFields.zipWithIndex.filter{case (f,index) => keyFields.contains(f)}.map(_._2).toSet
          r.map(cols => {
            cols.zipWithIndex.map{case (cell,index) => if (cell.notTotalValue && editableColIndices.contains(index)) {
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
       extraDisabledSubTotals ::: otherLayoutInfo.disabledSubTotals, table.fieldInfo, extraFormatInfo, false, previousPageData.map(_.columnNode))
    
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
      val (mainData, columnUOMs, cellUpdateInfoList) = nMainTableCells(rowData, cdX, extractUOMs)

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

          var columnsNotHandled = (0 until columnUOMs.length).toSet.filter(n => columnUOMs(n).toString.length() > 0)
          var currentRow = startRow
          while (columnsNotHandled.nonEmpty && (currentRow < colData.length)) {
            val spans = getSpans(colData(currentRow)).filter{case (start, end) => columnsNotHandled.contains(start)}
            spans.foreach{case (start, end) => {
              if ((start to end).map(c => columnUOMs(c)).distinct.size == 1) {
                val current = colData(currentRow)(start)
                val uom = columnUOMs(start).toString
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
      val rowDataArray = rowDataWithNullsAdded.map(_.toArray).toArray

      def axisCellUpdateInfo(cells:Array[Array[AxisCell]]) = {
        cells.zipWithIndex.flatMap{case (arrayOfCells, i) => arrayOfCells.zipWithIndex.flatMap{case (cell, j) => {
          if (cell.changed) Some(CellUpdateInfo(i, j, true, 0.0f)) else None
        }}}
      }
      val (rowAxisCellUpdateInfo, colAxisCellUpdateInfo) = previousPageData match {
        case None => (Nil,Nil)
        case Some(_) => (axisCellUpdateInfo(rowDataArray).toList, axisCellUpdateInfo(colData).toList)
      }

      PivotGrid(rowDataArray, colData, mainData, columnUOMs, cellUpdateInfoList, rowAxisCellUpdateInfo, colAxisCellUpdateInfo)
    }
  }

  private def nMainTableCells(flattenedRowValues:List[List[AxisCell]], flattenedColValues:List[List[AxisCell]], extractUOMs:Boolean = true) = {
    val aggregatedMainBucket = table.aggregatedMainBucket

    val measureFieldsEditable = table.editableInfo match {
      case Some(ei) => ei.blankCellsEditable
      case None => false
    }

    //create the main table looping through the flattened rows and columns and looking up the sums in mainTableBucket
    val allUnits = Array.fill(scala.math.max(1, flattenedColValues.size))(Set[UOM]())
    val dataAndCellInfo: Array[Array[(TableCell, Option[CellUpdateInfo])]] =
      (for ((rowValues, rowIndex) <- flattenedRowValues.zipWithIndex) yield {
        val rowSubTotal = rowValues.exists(_.totalState == SubTotal)
        val rowTotal = rowValues.exists(_.totalState == Total)
        val rowOtherValue = rowValues.exists(_.totalState == OtherValueTotal)
        (for ((columnValues, columnIndex) <- flattenedColValues.zipWithIndex) yield {
          val key = (rowValues.map(_.value.childKey).toList, columnValues.map(_.value.childKey).toList)

          def appendUOM(value:Any) {
            value match {
              case q:PivotQuantity => allUnits(columnIndex) = allUnits(columnIndex) ++ q.uoms
              case q:Quantity => allUnits(columnIndex) = allUnits(columnIndex) + q.uom
              case SpreadOrQuantity(Left(q)) =>  allUnits(columnIndex) = allUnits(columnIndex) + q.uom
              case SpreadOrQuantity(Right(sq)) =>  allUnits(columnIndex) = allUnits(columnIndex) + sq.uom
              case _ =>
            }
          }
          val measureField = columnValues.find(ac => ac.value.isMeasure)
          val measureCellOption = aggregatedMainBucket.get(key)
          val cellUpdateInfo = previousAggregatedMainBucket.map(pamb => {
            val previousCellOption = pamb.get(key)
            val matches = (previousCellOption == measureCellOption)
            if (!matches) {
              previousCellOption match {
                case Some(_) => CellUpdateInfo(rowIndex, columnIndex, matches, 0.0f)
                case None => CellUpdateInfo(rowIndex, columnIndex, true, 0.0f)
              }
            } else {
              CellUpdateInfo(rowIndex, columnIndex, matches, 0.0f)
            }
          })
          val tableCell = measureCellOption match {
            case Some(measureCell) => {
              measureCell.value match {
                case Some(s:Set[_]) => s.foreach(appendUOM)
                case Some(v) => appendUOM(v)
                case None =>
              }
              measureField match {
                case None => {
                  // This is probably a "fake" message cell.
                  measureCell.value match {
                    case Some(v) => TableCell(v)
                    case _ => {
                      if (measureCell.editable) {
                        TableCell.EditableNull
                      } else {
                        TableCell.Null
                      }
                    }
                  }
                }
                case Some(measureAxisCell) => {
                  val tc = measureCell.value match {
                    case None if measureCell.cellType == EditableCellState.Added => TableCell.UndefinedNew
                    case None => TableCell.Undefined
                    case Some(UndefinedValueNew) => TableCell.UndefinedNew
                    case Some(UndefinedValue) if measureCell.cellType == EditableCellState.Added => TableCell.UndefinedNew
                    case Some(UndefinedValue) => TableCell.Undefined
                    case Some(other) => table.fieldInfo.fieldToFormatter(measureAxisCell.value.field).format(other, extraFormatInfo)
                  }
                  tc.copy(state = measureCell.cellType, edits = measureCell.edits, originalValue = measureCell.originalValue, editable = measureCell.editable)
                }
              }
            }
            case None => {
              measureField match {
                case Some(ac) => {
                  if (measureFieldsEditable && table.editableInfo.get.measureFields.contains(ac.value.field)) {
                    TableCell.EditableNull
                  } else {
                    TableCell.Null
                  }
                }
                case None => TableCell.Null
              }
            }
          }

          val columnSubTotal = columnValues.exists(_.totalState == SubTotal)
          val columnTotal = columnValues.exists(_.totalState == Total)
          val columnOtherValue = columnValues.exists(_.totalState == OtherValueTotal)

          val tc = if ((rowTotal && columnSubTotal) || (columnTotal && rowSubTotal) || (rowTotal && columnTotal) || (rowSubTotal && columnSubTotal)) {
            tableCell.copy(totalState=SubtotalTotal)
          } else if (rowTotal || columnTotal) {
            tableCell.copy(totalState=Total)
          } else if (rowSubTotal || columnSubTotal) {
            tableCell.copy(totalState=SubTotal)
          } else if (rowOtherValue || columnOtherValue) {
            tableCell.copy(totalState=OtherValueTotal)
          } else {
            tableCell
          }
          (tc, cellUpdateInfo)
        }).toArray
      }).toArray

    val data = dataAndCellInfo.map(_.map(_._1))
    val cellUpdateInfo = dataAndCellInfo.flatMap(_.flatMap(_._2)).toList.filter(!_.matches)

    if (extractUOMs) {
      // If a column only has one uom, set that uom as the column header.
      for ((row, rowIndex) <- data.zipWithIndex) {
        for ((value,colIndex) <- data(rowIndex).zipWithIndex) {
          val uomSet = allUnits(colIndex)
          if (uomSet.size == 1) {
            val newText = value.value match {
              case q:PivotQuantity => PivotFormatter.shortAndLongText(q, extraFormatInfo, false)._1
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
        uomSet.iterator.next()
      } else {
        UOM.NULL
      }
    })


    (data, columnUOMs, cellUpdateInfo)
  }

  def toSTable(name:String) = {
    val (rowHeaderCells, columnHeaderCells, mainTableCells) = allTableCells(false)
    val rowHeader= if (rowHeaderCells.isEmpty) List() else rowHeaderCells(0).map { cv => SColumn(cv.value.field.name) }.toList
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