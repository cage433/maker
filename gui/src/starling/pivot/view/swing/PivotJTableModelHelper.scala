package starling.pivot.view.swing

import fieldchoosers.RowComponent
import starling.quantity.UOM
import starling.pivot._
import javax.swing.table.AbstractTableModel
import java.awt.{Dimension, Graphics, Component => AWTComp, Color, KeyboardFocusManager}
import collection.mutable.{ListBuffer, HashMap}
import model._
import scala.Array
import javax.swing._
import scala.swing.Swing._
import swing.{ScrollPane, ListView, Panel}
import swing.event.{MouseClicked, KeyPressed, KeyTyped}
import starling.pivot.EditableCellState._
import scala.collection.mutable.{HashMap => MMap}
import collection.immutable.Map
import org.jdesktop.swingx.JXTable

case class OverrideDetails(text:String, state:EditableCellState)

abstract class PivotJTableModel extends AbstractTableModel {
  def paintTable(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int)
  def rowHeader(row:Int, col:Int):Boolean
  def mapCellToFields(row:Int, col:Int):List[(Field, Selection)]
  def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView)
  def deleteCells(cells:List[(Int,Int)])
  def resetCells(cells:List[(Int,Int)])
  def textTyped(textField:JTextField, cellEditor:CellEditor , r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:AWTComp)
  def finishedEditing()
  def popupShowing:Boolean
  def focusPopup()
  def acceptableValues(r:Int, c:Int):Set[String]

  protected var overrideMap = new MMap[(Int,Int),OverrideDetails]()
  def revert(table:PivotJTable):Boolean = {
    overrideMap.clear()
    false
  }
}

class PivotJTableModelHelper(var data0:Array[Array[TableCell]],
                     editableInfo:Option[EditableInfo],
                     var rowHeaderData0:Array[Array[AxisCell]],
                     var colHeaderData0:Array[Array[AxisCell]],
                     var uoms0:Array[UOM],
                     resizeMainTableColumns: =>Unit,
                     resizeRowHeaderTableColumns: =>Unit,
                     fieldState:PivotFieldsState,
                     var extraFormatInfo:ExtraFormatInfo,
                     pivotEdits:PivotEdits,
                     updateEdits:(PivotEdits) => Unit,
                     formatInfo:FormatInfo) {
  private var mainColCount0 = data0(0).length
  private var rowHeaderColCount0 = rowHeaderData0(0).length
  private var colHeaderRowCount0 = colHeaderData0.length

  private val extraLine = editableInfo match {
    case None => false
    case Some(info) => info.extraLine
  }

  def setData(rd:Array[Array[AxisCell]], cd:Array[Array[AxisCell]], d:Array[Array[TableCell]], extraFormatInfo:ExtraFormatInfo) {
    this.extraFormatInfo = extraFormatInfo
    data0 = d
    rowHeaderData0 = rd
    colHeaderData0 = cd
    mainTableModel.fireTableRowsUpdated(0, mainTableModel.getRowCount-1)
    rowHeaderTableModel.fireTableRowsUpdated(0, rowHeaderTableModel.getRowCount-1)
    colHeaderTableModel.fireTableRowsUpdated(0, colHeaderTableModel.getRowCount-1)
    fullTableModel.fireTableRowsUpdated(0, fullTableModel.getRowCount-1)
    resizeRowHeaderTableColumns
    resizeMainTableColumns
  }

  def uoms = uoms0
  def uoms_=(u:Array[UOM]) {uoms0 = u}

  private def singleValueFilters() = {
    Map() ++ fieldState.filterAreaFields.flatMap(f => {
      val (field, selection) = fieldState.filters.find{case (f0,sel) => f == f0}.get
      selection match {
        case s@SomeSelection(v) if v.size == 1 => Some((field -> v.iterator.next))
        case _ => None
      }
    })
  }

  private def initializedBlankRow() = {
    val keyFields = editableInfo.get.editableKeyFields.keySet
    (Map() ++ (keyFields.map(f => {f -> UndefinedValue}))) ++ singleValueFilters
  }

  val rowHeaderTableModel = new PivotJTableModel {
    private val addedRows0 = new ListBuffer[Array[AxisCell]]
    private val blankCells = rowHeaderData0(0).map(av => {
      val newAV = av.value.copy(value = BlankAddedAxisValueType)
      av.copy(value = newAV, label = "", collapsible = None)
    })
    if (extraLine) {
      addedRows0 += blankCells
    }

    def addRow() {addedRows0 += blankCells}

    override def revert(table:PivotJTable) = {
      super.revert(table)
      if (addedRows0.size > 1) {
        val selectedColumn = table.getSelectedColumn
        val r = getRowCount - 1
        addedRows0.clear()
        addedRows0 += blankCells
        fireTableRowsDeleted(r, r)
        table.setSelectedCells(List((r-1, selectedColumn)))
        true
      } else {
        false
      }
    }

    def getRowCount = {rowHeaderData0.length + addedRows0.length}
    def getColumnCount = rowHeaderData0(0).length
    def getValueAt(rowIndex:Int, columnIndex:Int):AxisCell = {
      if (rowIndex < rowHeaderData0.length) {
        if (overrideMap.contains((rowIndex, columnIndex))) {
          val details = overrideMap((rowIndex, columnIndex))
          rowHeaderData0(rowIndex)(columnIndex).copy(label = details.text, overrideState = Some(details.state))
        } else {
          rowHeaderData0(rowIndex)(columnIndex)
        }
      } else {
        if (overrideMap.contains((rowIndex, columnIndex))) {
          val details = overrideMap((rowIndex, columnIndex))
          addedRows0(0)(columnIndex).copy(label = details.text, overrideState = Some(details.state))
        } else {
          addedRows0(0)(columnIndex)
        }
      }
    }
    def paintTable(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) {
      PivotTableUI.rowHeaderPaintGrid(g, table, rMin, rMax, cMin, cMax, getColumnCount - 1)
      PivotTableUI.rowHeaderPaintCells(g, table, rendererPane, rMin, rMax, cMin, cMax)
    }
    def rowHeader(row:Int,col:Int) = true
    def mapCellToFields(row:Int, col:Int) = List()
    def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView) {
      val path = (0 to col).map(colIndex => {getValueAt(row, colIndex).value}).toList
      pivotTableView.collapseOrExpandRow(path)
    }

    private def key(rowIndex:Int, columnIndex:Int) = {
      val filterFieldToValues = Map() ++ fieldState.filterAreaFields.flatMap(f => {
        val (field, selection) = fieldState.filters.find{case (f0,sel) => f == f0}.get
        selection match {
          case s@SomeSelection(v) if v.size == 1 => Some((field -> s))
          case _ => None
        }
      })

      val rowFilters = Map() ++ fieldState.filters.collect{case (f,s:SomeSelection) if fieldState.rowFields.contains(f) => (f -> s)}
      val columnFilters = Map() ++ fieldState.filters.collect{case (f,s:SomeSelection) if fieldState.columns.allFields.contains(f) => (f -> s)}

      val rowHeaderCells = rowHeaderData0(rowIndex).take(columnIndex+1)
      val rowHeaderFieldToValues = Map() ++ rowHeaderCells.map(cell => (cell.value.field -> SomeSelection(Set(cell.value.value.originalValue.getOrElse(cell.value.value.value)))))

      rowHeaderFieldToValues ++ filterFieldToValues ++ rowFilters ++ columnFilters
    }

    def deleteCells(cells:List[(Int,Int)]) {
      var edits = pivotEdits
      cells.foreach{case (r,c) => {
        if (r < rowHeaderData0.length) {
          val value = getValueAt(r, c)
          if (value.state != EditableCellState.Added) {
            val labelToUse = value.value.value.originalValue match {
              case Some(origValue) => {
                formatInfo.fieldToFormatter(value.value.field).format(origValue, extraFormatInfo).text
              }
              case None => value.label
            }
            overrideMap((r,c)) = OverrideDetails(labelToUse, Deleted)
            edits = edits.withDelete(KeyFilter(key(r, c)), value.value.field)
          } else {
            overrideMap((r,c)) = OverrideDetails("", Added)
            val rowIndex = value.value.childKey.value.asInstanceOf[NewRowValue].rowIndex
            edits = edits.withNewAmended(rowIndex, value.value.field, None)
          }
        }
      }}
      if (edits != pivotEdits) {
        updateEdits(edits)
      }
    }

    def resetCells(cells:List[(Int,Int)]) {
      var edits = pivotEdits
      cells.map{case (r,c) => {
        val v = getValueAt(r, c)
        val labelToUse = v.value.value.originalValue match {
          case None => v.label
          case Some(origVal) => formatInfo.fieldToFormatter(v.value.field).format(origVal, extraFormatInfo).text
        }
        overrideMap((r,c)) = OverrideDetails(labelToUse, Normal)
        val editsForCell = v.value.pivotEdits
        edits = edits.remove(editsForCell)
      }}
      if (edits != pivotEdits) {
        updateEdits(edits)
      }
    }

    override def isCellEditable(rowIndex:Int, columnIndex:Int) = {
      val v = getValueAt(rowIndex, columnIndex)
      v.editable && (v.state != EditableCellState.Deleted)
    }
    override def setValueAt(value:AnyRef, rowIndex:Int, columnIndex:Int) {
      val s = value.asInstanceOf[String].trim
      // Using a zero row index here as it doesn't really matter as long as it is the correct column.
      val rowHeaderField = rowHeaderData0(0)(columnIndex).value.field
      val parser = editableInfo.get.editableKeyFields(rowHeaderField)

      val (newValue,newLabel) = if (s.isEmpty) (None, "") else {
        val (v,t) = parser.parse(s)
        (Some(v), t)
      }

      if (rowIndex < rowHeaderData0.length) {
        val currentCell = getValueAt(rowIndex, columnIndex)

        val originalLabel = currentCell.value.value.originalValue match {
          case None => "sfkjfhxcjkvuivyruvhrzzasaf$%£$££"
          case Some(origVal) => formatInfo.fieldToFormatter(currentCell.value.field).format(origVal, extraFormatInfo).text
        }

        if (originalLabel == newLabel) {
          resetCells(List((rowIndex, columnIndex)))
        } else if (currentCell.text != newLabel) {
          val stateToOverride = if (currentCell.state == EditableCellState.Added) EditableCellState.Added else EditableCellState.Edited
          overrideMap((rowIndex, columnIndex)) = OverrideDetails(newLabel, stateToOverride)
          currentCell.value.childKey.value match {
            case NewRowValue(ri) => updateEdits(pivotEdits.withNewAmended(ri, rowHeaderField, newValue))
            case _ => updateEdits(pivotEdits.withAmend(KeyFilter(key(rowIndex, columnIndex)), rowHeaderField, newValue))
          }
        }
      } else {
        overrideMap((rowIndex, columnIndex)) = OverrideDetails(newLabel, EditableCellState.Added)
        newValue.foreach { nv => {     
          addRowToTables()
          val row = initializedBlankRow + (rowHeaderField -> nv)
          updateEdits(pivotEdits.withAddedRow(row))
        } }
      }
    }

    def acceptableValues(r:Int, c:Int) = {
      val rowHeaderField = rowHeaderData0(0)(c).value.field
      val parser = editableInfo.get.editableKeyFields(rowHeaderField)
      parser.acceptableValues
    }

    def textTyped(textField:JTextField, cellEditor:CellEditor, r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:AWTComp) {
      // Using a zero row index here as it doesn't really matter as long as it is the correct column.
      val vals = acceptableValues(0, c)
      if (vals.nonEmpty) {
        val t = textField.getText.toLowerCase
        val filteredVals = vals.filter(_.toLowerCase.startsWith(t))
        val sortedVals = {
          if (filteredVals.nonEmpty) {
            filteredVals.toList.sortWith(_.toLowerCase < _.toLowerCase)
          } else {
            vals.toList.sortWith(_.toLowerCase < _.toLowerCase)
          }
        }
        val currentListData = popupListView.listData.toList
        if (currentListData != sortedVals) {
          popupListView.listData = sortedVals
        }
        if (!popupMenu.isShowing) {
          val selectionAtTimeOfPopup = textField.getSelectedText
          val caretPositionAtTimeOfPopup = textField.getCaretPosition

          popupMenu.setMinimumSize(viewScrollPane.preferredSize)
          popupMenu.show(textField, cellEditor, tableFrom, textField, 0, textField.getSize().height-1)
          focusOwner.map(_.requestFocusInWindow())

          onEDT(onEDT({
            val currentSelection = textField.getSelectedText
            if (selectionAtTimeOfPopup == null && currentSelection != null) {
              // It is likely we are on windows and we need to do something funky so that we don't select the whole text field.
              val l = textField.getText.length()
              val posToUse = if (caretPositionAtTimeOfPopup <= l) caretPositionAtTimeOfPopup else l
              textField.setCaretPosition(posToUse)
            }
          }))
        }
      }
    }
    def finishedEditing() {popupMenu setVisible false}
    def popupShowing = popupMenu.isShowing
    def focusPopup() {
      KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu)
      popupListView.requestFocusInWindow()
      popupListView.selectIndices(0)
    }
  }

  val colHeaderTableModel = new PivotJTableModel {
    def getRowCount = colHeaderData0.length
    def getColumnCount = colHeaderData0(0).length
    def getValueAt(rowIndex:Int, columnIndex:Int):AxisCell = {
      if (overrideMap.contains((rowIndex, columnIndex))) {
        val details = overrideMap((rowIndex, columnIndex))
        colHeaderData0(rowIndex)(columnIndex).copy(label = details.text, overrideState = Some(details.state))
      } else {
        colHeaderData0(rowIndex)(columnIndex)
      }
    }
    def paintTable(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) {
      PivotTableUI.colHeaderPaintGrid(g, table, rMin, rMax, cMin, cMax)
      PivotTableUI.colHeaderPaintCells(g, table, rendererPane, rMin, rMax, cMin, cMax)
    }
    def rowHeader(row:Int,col:Int) = false
    def mapCellToFields(row:Int, col:Int) = List()
    def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView) {
      val path = (0 to row).map(rowIndex => {getValueAt(rowIndex, col).asInstanceOf[AxisCell].value}).toList
      pivotTableView.collapseOrExpandCol(path)
    }
    def deleteCells(cells:List[(Int,Int)]) {}
    def resetCells(cells:List[(Int,Int)]) {
      var edits = pivotEdits
      cells.map{case (r,c) => {
        val v = getValueAt(r, c)
        val labelToUse = v.value.value.originalValue match {
          case None => v.label
          case Some(origVal) => formatInfo.fieldToFormatter(v.value.field).format(origVal, extraFormatInfo).text
        }
        overrideMap((r,c)) = OverrideDetails(labelToUse, Normal)
        val editsForCell = v.value.pivotEdits
        edits = edits.remove(editsForCell)
      }}
      if (edits != pivotEdits) {
        updateEdits(edits)
      }
    }
    def acceptableValues(r:Int, c:Int) = Set.empty
    def textTyped(textField:JTextField, cellEditor:CellEditor, r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:AWTComp) {}
    def finishedEditing() {popupMenu setVisible false}
    def popupShowing = popupMenu.isShowing
    def focusPopup() {
      KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu)
      popupListView.requestFocusInWindow()
      popupListView.selectIndices(0)
    }
  }

  val blankAxisCellTableModel = new PivotJTableModel {
    def getRowCount = colHeaderData0.length
    def getColumnCount = rowHeaderData0(0).length
    def getValueAt(rowIndex:Int, columnIndex:Int) = AxisCell.Null
    def paintTable(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) {}
    def rowHeader(row:Int,col:Int) = false
    def mapCellToFields(row:Int, col:Int) = List()
    def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView) {}
    def deleteCells(cells:List[(Int,Int)]) {}
    def resetCells(cells:List[(Int,Int)]) {}
    def acceptableValues(r:Int, c:Int) = Set.empty
    def textTyped(textField:JTextField, cellEditor:CellEditor, r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:AWTComp) {}
    def finishedEditing() {popupMenu setVisible false}
    def popupShowing = popupMenu.isShowing
    def focusPopup() {
      KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu)
      popupListView.requestFocusInWindow()
      popupListView.selectIndices(0)
    }
  }

  private def addRowToTables() {
    val rowHeader = rowHeaderTableModel.getRowCount
    val main = mainTableModel.getRowCount
    val full = fullTableModel.getRowCount

    rowHeaderTableModel.addRow()
    mainTableModel.addRow()

    rowHeaderTableModel.fireTableRowsInserted(rowHeader, rowHeader)
    mainTableModel.fireTableRowsInserted(main, main)
    fullTableModel.fireTableRowsInserted(full, full)
  }

  val mainTableModel = new PivotJTableModel {
    private val addedRows0 = new ListBuffer[Array[TableCell]]
    private val blankCells = data0(0).map(_.copy(state = EditableCellState.Added, text = ""))
    if (extraLine) {
      addedRows0 += blankCells
    }

    def addRow() {addedRows0 += blankCells}

    override def revert(table:PivotJTable) = {
      super.revert(table)
      if (addedRows0.size > 1) {
        val selectedColumn = table.getSelectedColumn
        val r = getRowCount - 1
        addedRows0.clear()
        addedRows0 += blankCells
        fireTableRowsDeleted(r, r)
        table.setSelectedCells(List((r-1, selectedColumn)))
        true
      } else {
        false
      }
    }

    def getRowCount = data0.length + addedRows0.length
    def getColumnCount = data0(0).length
    def getValueAt(rowIndex:Int, columnIndex:Int):TableCell = {
      if (rowIndex < data0.length) {
        if (overrideMap.contains((rowIndex, columnIndex))) {
          val details = overrideMap((rowIndex, columnIndex))
          data0(rowIndex)(columnIndex).copy(text = details.text, state = details.state)
        } else {
          data0(rowIndex)(columnIndex)
        }
      } else {
        if (overrideMap.contains((rowIndex, columnIndex))) {
          val details = overrideMap((rowIndex, columnIndex))
          addedRows0(0)(columnIndex).copy(text = details.text, state = details.state)
        } else {
          addedRows0(0)(columnIndex)
        }
      }
    }
    override def isCellEditable(rowIndex:Int, columnIndex:Int) = {
      val v = getValueAt(rowIndex, columnIndex)
      v.editable && (v.state != EditableCellState.Deleted)
    }
    def resetCells(cells:List[(Int,Int)]) {
      var edits = pivotEdits
      cells.map{case (r,c) => {
        val cell = getValueAt(r, c)
        val labelToUse = cell.originalValue match {
          case None => cell.text
          case Some(origValue) => {
            colHeaderData0.find(_(c).value.isMeasure) match {
              case None => cell.text
              case Some(arr) => {
                val measureInfo = arr(c)
                formatInfo.fieldToFormatter(measureInfo.value.field).format(origValue, extraFormatInfo).text
              }
            }
          }
        }
        overrideMap((r,c)) = OverrideDetails(labelToUse, Normal)
        val editsForCell = cell.edits
        edits = edits.remove(editsForCell)
      }}
      if (edits != pivotEdits) {
        updateEdits(edits)
      }
    }
    def deleteCells(cells:List[(Int,Int)]) {
      var edits = pivotEdits
      cells.foreach{case (r,c) => {
        if (r < rowHeaderData0.length) {
          val value = getValueAt(r, c)
          colHeaderData0.find(_(c).value.isMeasure).foreach(arr => {
            val measureInfo = arr(c)
            if (value.state != EditableCellState.Added) {
              val labelToUse = value.originalValue match {
                case Some(origValue) => {
                  formatInfo.fieldToFormatter(measureInfo.value.field).format(origValue, extraFormatInfo).text
                }
                case None => value.text
              }
              overrideMap((r,c)) = OverrideDetails(labelToUse, Deleted)
              edits = edits.withAmend(KeyFilter(key(r, c, measureInfo)), measureInfo.value.field, None)
            } else {
              overrideMap((r,c)) = OverrideDetails("", Added)
              val rowCellForCheck = rowHeaderData0(r)(0)
              val rowIndex = rowCellForCheck.value.childKey.value.asInstanceOf[NewRowValue].rowIndex
              edits = edits.withNewAmended(rowIndex, measureInfo.value.field, None)
            }
          })
        }
      }}
      if (edits != pivotEdits) {
        updateEdits(edits)
      }
    }

    private def key(rowIndex:Int, columnIndex:Int, measureInfo:AxisCell):Map[Field, SomeSelection] = {
      val filterFieldToValues = Map() ++ fieldState.filterAreaFields.flatMap(f => {
        val (field, selection) = fieldState.filters.find{case (f0,sel) => f == f0}.get
        selection match {
          case s@SomeSelection(v) if v.size == 1 => Some((field -> s))
          case _ => None
        }
      })

      val rowHeaderCells = rowHeaderData0(rowIndex)
      val rowHeaderFieldToValues = Map() ++ rowHeaderCells.map(cell => (cell.value.field -> SomeSelection(Set(cell.value.value.originalValue.getOrElse(cell.value.value.value)))))

      val colHeaderFieldToValues = Map() ++ colHeaderData0.map(r => {
        val cell = r(columnIndex)
        (cell.value.field -> SomeSelection(Set(cell.value.value.value)))
      }).filterNot{case (field,_) => (measureInfo.value.field == field) || (field == Field.NullField)}

      rowHeaderFieldToValues ++ colHeaderFieldToValues ++ filterFieldToValues
    }

    override def setValueAt(value:AnyRef, rowIndex:Int, columnIndex:Int) {
      val s = value.asInstanceOf[String].trim
      val measureInfo = colHeaderData0.find(_(columnIndex).value.isMeasure).get(columnIndex)
      val parser = editableInfo.get.editableMeasures(measureInfo.value.field)
      
      val (newValue,newLabel) = if (s.isEmpty) (None, "") else {
        val (v,l) = parser.parse(s)
        v match {
          case pq:PivotQuantity if uoms0.length > columnIndex => {
            val uom = uoms0(columnIndex)
            val dv = pq.doubleValue.get
            val newPQ = new PivotQuantity(dv, uom)
            (Some(newPQ), PivotFormatter.formatPivotQuantity(newPQ, extraFormatInfo, false))
          }
          case _ => (Some(v),l)
        }
      }

      if (rowIndex < rowHeaderData0.length) {
        val currentCell = getValueAt(rowIndex, columnIndex)

        val originalLabel = currentCell.originalValue match {
          case None => "sfkjfhxcjkvuivyruvhrzzasaf$%£$££"
          case Some(origVal) => formatInfo.fieldToFormatter(measureInfo.value.field).format(origVal, extraFormatInfo).text
        }
        if (originalLabel == newLabel) {
          resetCells(List((rowIndex, columnIndex)))
        } else if (currentCell.text != newLabel) {
          if (currentCell.value == NoValue) {
            overrideMap((rowIndex, columnIndex)) = OverrideDetails(newLabel, EditableCellState.Added)
            newValue.foreach(nv => {
              val row = key(rowIndex, columnIndex, measureInfo).mapValues(_.values.head) + (measureInfo.value.field -> nv)
              updateEdits(pivotEdits.withAddedRow(row))
            })
          } else {
            val stateToOverride = if (currentCell.state == EditableCellState.Added) EditableCellState.Added else EditableCellState.Edited
            overrideMap((rowIndex, columnIndex)) = OverrideDetails(newLabel, stateToOverride)
            val rowCellForCheck = rowHeaderData0(rowIndex)(0)
            rowCellForCheck.value.childKey.value match {
              case NewRowValue(ri) => updateEdits(pivotEdits.withNewAmended(ri, measureInfo.value.field, newValue))
              case _ => updateEdits(pivotEdits.withAmend(KeyFilter(key(rowIndex, columnIndex, measureInfo)), measureInfo.value.field, newValue))
            }
          }
        }
      } else {
        overrideMap((rowIndex, columnIndex)) = OverrideDetails(newLabel, EditableCellState.Added)
        newValue.foreach(nv => {
          addRowToTables()
          val row = initializedBlankRow + (measureInfo.value.field -> nv)
          updateEdits(pivotEdits.withAddedRow(row))
        })
      }
    }

    def paintTable(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) {
      PivotTableUI.mainPaintGrid(g,table,rMin,rMax,cMin,cMax)
      PivotTableUI.rowHeaderPaintCells(g,table,rendererPane,rMin,rMax,cMin,cMax)
    }
    def rowHeader(row:Int,col:Int) = false
    def mapCellToFields(row:Int, col:Int) = {
      val rowFilters = (0 until rowHeaderTableModel.getColumnCount).flatMap {
        i => {
          val axisCell = rowHeaderTableModel.getValueAt(row, i).asInstanceOf[AxisCell]
          axisCell.value.value match {
            case ValueAxisValueType(v) => Some(axisCell.value.field -> SomeSelection(Set() + v))
            case _ => None
          }
        }
      }
      val columnFilters = (0 until colHeaderTableModel.getRowCount).flatMap {
        i => {
          val axisCell = colHeaderTableModel.getValueAt(i, col).asInstanceOf[AxisCell]
          axisCell.value.value match {
            case ValueAxisValueType(v) => Some(axisCell.value.field -> SomeSelection(Set() + v))
            case _ => None
          }
        }
      }
      rowFilters.toMap.toList ::: columnFilters.toMap.toList
    }
    def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView) {}

    def acceptableValues(r:Int, c:Int) = {
      val measureInfo = colHeaderData0.find(_(c).value.isMeasure).get(c)
      val parser = editableInfo.get.editableMeasures(measureInfo.value.field)
      parser.acceptableValues
    }

    def textTyped(textField:JTextField, cellEditor:CellEditor, r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:AWTComp) {
      val vals = acceptableValues(0, c)
      if (vals.nonEmpty) {
        val t = textField.getText.toLowerCase
        val filteredVals = vals.filter(_.toLowerCase.startsWith(t))
        val sortedVals = {
          if (filteredVals.nonEmpty) {
            filteredVals.toList.sortWith(_.toLowerCase < _.toLowerCase)
          } else {
            vals.toList.sortWith(_.toLowerCase < _.toLowerCase)
          }
        }
        val currentListData = popupListView.listData.toList
        if (currentListData != sortedVals) {
          popupListView.listData = sortedVals
        }
        if (!popupMenu.isShowing) {
          val selectionAtTimeOfPopup = textField.getSelectedText
          val caretPositionAtTimeOfPopup = textField.getCaretPosition

          popupMenu.setMinimumSize(viewScrollPane.preferredSize)
          popupMenu.show(textField, cellEditor, tableFrom, textField, 0, textField.getSize().height-1)
          focusOwner.map(_.requestFocusInWindow())

          onEDT(onEDT({
            val currentSelection = textField.getSelectedText
            if (selectionAtTimeOfPopup == null && currentSelection != null) {
              // It is likely we are on windows and we need to do something funky so that we don't select the whole text field.
              val l = textField.getText.length()
              val posToUse = if (caretPositionAtTimeOfPopup <= l) caretPositionAtTimeOfPopup else l
              textField.setCaretPosition(posToUse)
            }
          }))
        }
      }
    }
    def finishedEditing() {popupMenu setVisible false}
    def popupShowing = popupMenu.isShowing
    def focusPopup() {
      KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu)
      popupListView.requestFocusInWindow()
      popupListView.selectIndices(0)
    }
  }

  private val popupMenu = new JPopupMenu {
    var editor:JTextField = _
    var cellEditor:CellEditor = _
    var tableToFocus:AWTComp = _

    def show(ed:JTextField, cEditor:CellEditor, tToFocus:AWTComp, invoker:AWTComp, x:Int, y:Int) {
      editor = ed
      cellEditor = cEditor
      tableToFocus = tToFocus
      show(invoker, x, y)
    }
  }
  private val popupListView = new ListView(List(" ")) {
    fixedCellWidth = 300
    selection.intervalMode = ListView.IntervalMode.Single
    peer.setFocusTraversalKeysEnabled(false)
    def selectText(t:String) {
      popupMenu.editor.setText(t)
      popupMenu.cellEditor.stopCellEditing()
      popupMenu.tableToFocus.requestFocusInWindow()
    }
    reactions += {
      case KeyPressed(_,scala.swing.event.Key.Enter,_,_) => selectText(selection.items.head)
      case KeyPressed(_,scala.swing.event.Key.Tab,_,_) => selectText(selection.items.head)
      case e@KeyPressed(_,scala.swing.event.Key.BackSpace,_,_) => e.consume()
      case e@KeyPressed(_,scala.swing.event.Key.Escape,_,_) => {
        e.consume()
        selectIndices(-1)
        popupMenu.editor.requestFocusInWindow()
        onEDT({
          popupMenu.editor.setCaretPosition(popupMenu.editor.getText.length())
        })
      }
      case KeyPressed(_,scala.swing.event.Key.Up,_,_) if selection.indices.head == 0 => {
        selectIndices(-1)
        popupMenu.editor.requestFocusInWindow()
        onEDT({
          popupMenu.editor.setCaretPosition(popupMenu.editor.getText.length())
        })
      }
      case e@KeyPressed(_,scala.swing.event.Key.Z,scala.swing.event.Key.Modifier.Control,_) => {
        e.consume()
        selectIndices(-1)
        popupMenu.editor.requestFocusInWindow()
        onEDT({
          popupMenu.editor.setCaretPosition(popupMenu.editor.getText.length())
        })
      }
      case MouseClicked(_,_,_,1,_) => selectText(selection.items.head)
    }
    listenTo(keys, mouse.clicks)
  }
  private val viewScrollPane = new ScrollPane(popupListView) {
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
  }
  popupMenu.add(viewScrollPane.peer)
  popupMenu.setBorder(EmptyBorder)

  private val allModels = List(rowHeaderTableModel, colHeaderTableModel, mainTableModel, blankAxisCellTableModel)

  val fullTableModel = new PivotJTableModel {
    def getRowCount = colHeaderTableModel.getRowCount + rowHeaderTableModel.getRowCount
    def getColumnCount = rowHeaderTableModel.getColumnCount + mainTableModel.getColumnCount

    def getValueAt(rowIndex:Int, columnIndex:Int) = {
      val (m,r,c) = getTableModel(rowIndex, columnIndex)
      m.getValueAt(r,c)
    }
    override def isCellEditable(rowIndex:Int, columnIndex:Int) = {
      val (m,r,c) = getTableModel(rowIndex, columnIndex)
      m.isCellEditable(r,c)
    }
    override def setValueAt(aValue:AnyRef, rowIndex:Int, columnIndex:Int) {
      val (m,r,c) = getTableModel(rowIndex, columnIndex)
      m.setValueAt(aValue,r,c)
    }
    def deleteCells(cells:List[(Int,Int)]) {
      val modelMap = getTableModels(cells)
      modelMap.foreach{case (mod, list) => {
        mod.deleteCells(list.map{case (m,r,c) => (r,c)})
      }}
    }
    def resetCells(cells:List[(Int,Int)]) {
      val modelMap = getTableModels(cells)
      modelMap.foreach{case (mod, list) => {
        mod.resetCells(list.map{case (m,r,c) => (r,c)})
      }}
    }
    private def getTableModels(cells:List[(Int,Int)]) = {
      cells.map{case (r,c) => {
        getTableModel(r,c)
      }}.groupBy(_._1)
    }

    private def getTableModel(rowIndex:Int, columnIndex:Int) = {
      if ((rowIndex >= colHeaderTableModel.getRowCount) && (columnIndex < rowHeaderTableModel.getColumnCount)) {
        (rowHeaderTableModel, rowIndex - colHeaderTableModel.getRowCount, columnIndex)
      } else if ((rowIndex < colHeaderTableModel.getRowCount) && (columnIndex >= rowHeaderTableModel.getColumnCount)) {
        (colHeaderTableModel, rowIndex, columnIndex - rowHeaderTableModel.getColumnCount)
      } else if ((rowIndex >= colHeaderTableModel.getRowCount)) {
        (mainTableModel, rowIndex - colHeaderTableModel.getRowCount, columnIndex - rowHeaderTableModel.getColumnCount)
      } else {
        (blankAxisCellTableModel, 0, 0)
      }
    }

    def paintTable(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) {
      {
        // Paint the row header table.
        val rMinL = math.max(rMin, colHeaderRowCount0)
        val rMaxL = math.max(rMax, colHeaderRowCount0)
        val cMinL = math.min(rowHeaderColCount0, cMin)
        val cMaxL = math.min(rowHeaderColCount0 - 1, cMax)

        PivotTableUI.rowHeaderPaintGrid(g, table, rMinL, rMaxL, cMinL, cMaxL, rowHeaderTableModel.getColumnCount - 1)
        PivotTableUI.rowHeaderPaintCells(g, table, rendererPane, rMinL, rMaxL, cMinL, cMaxL)
      }
      {
        // Paint the col header table.
        val rMinL = math.min(colHeaderRowCount0, rMin)
        val rMaxL = math.min(colHeaderRowCount0 - 1, rMax)
        val cMinL = math.max(rowHeaderColCount0, cMin)
        val cMaxL = math.max(rowHeaderColCount0, cMax)

        PivotTableUI.colHeaderPaintGrid(g, table, rMinL, rMaxL, cMinL, cMaxL)
        PivotTableUI.colHeaderPaintCells(g, table, rendererPane, rMinL, rMaxL, cMinL, cMaxL)
      }
      {
        // Paint the main table.
        val rMinL = math.max(colHeaderRowCount0, rMin)
        val rMaxL = math.max(colHeaderRowCount0, rMax)
        val cMinL = math.max(rowHeaderColCount0, cMin)
        val cMaxL = math.max(rowHeaderColCount0, cMax)

        PivotTableUI.mainPaintGrid(g,table,rMinL, rMaxL, cMinL, cMaxL)
        PivotTableUI.rowHeaderPaintCells(g,table,rendererPane,rMinL, rMaxL, cMinL, cMaxL)
      }
    }
    def rowHeader(row:Int,col:Int) = ((col < rowHeaderColCount0) && (row >= colHeaderRowCount0))
    def mapCellToFields(row:Int, col:Int) = {
      val offsetRow = row - colHeaderRowCount0
      val offsetCol = col - rowHeaderColCount0
      if ((offsetRow >= 0) && (offsetCol >= 0)) {
        mainTableModel.mapCellToFields(offsetRow, offsetCol)
      } else {
        List()
      }
    }
    def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView) {
      if (col < rowHeaderColCount0) {
        rowHeaderTableModel.collapseOrExpand(row - colHeaderRowCount0, col, pivotTableView)
      } else {
        colHeaderTableModel.collapseOrExpand(row, col - rowHeaderColCount0, pivotTableView)
      }
    }

    override def fireTableStructureChanged() {
      super.fireTableStructureChanged()
      allModels.foreach(_.fireTableStructureChanged())
    }

    override def fireTableDataChanged() {
      super.fireTableDataChanged()
      allModels.foreach(_.fireTableDataChanged())
    }


    def acceptableValues(r:Int, c:Int) = {
      val (m,row,col) = getTableModel(r, c)
      m.acceptableValues(row, col)
    }

    def textTyped(textField:JTextField, cellEditor:CellEditor, r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:AWTComp) {
      val (m,row,col) = getTableModel(r, c)
      m.textTyped(textField, cellEditor, row, col, focusOwner, tableFrom)
    }
    def finishedEditing() {popupMenu setVisible false}
    def popupShowing = popupMenu.isShowing
    def focusPopup() {
      KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu)
      popupListView.requestFocusInWindow()
      popupListView.selectIndices(0)
    }
  }

  def resizeRowHeaderColumns(fullTable:JTable, rowHeaderTable:JTable, rowComponent:RowComponent,
                             rowFieldHeadingCount:Array[Int], sizerPanel:Panel, rowHeaderScrollPane:JScrollPane) {
    def getRowHeaderMaxColumnWidth(col:Int) = {
      val numRows = rowHeaderTable.getRowCount
      val tmpLabel = new JLabel("")
      var max = -1
      var anyCollapsible = false
      for (row <- 0 until numRows) {
        val axisCell = rowHeaderTable.getValueAt(row, col).asInstanceOf[AxisCell]
        val width = {
          tmpLabel.setFont(MainTableCellRenderer.selectFont(axisCell.value.value.value))
          tmpLabel.setText(axisCell.text)
          if (axisCell.collapsible.isDefined) {
            anyCollapsible = true
          }
          tmpLabel.getPreferredSize.width
        }
        //used to use minColumnWidth
        if (width > max) {
          max = width
        }
      }
      max + (if (anyCollapsible) MainTableCellRenderer.LeftIconWidth + 6 else 0)
    }

    val fullColumnModel = fullTable.getColumnModel
    val rowHeaderColumnModel = rowHeaderTable.getColumnModel
    val numCols = rowHeaderColCount0
    var col = 0
    if ((rowComponent.numberOfFields == 0) ||
            ((numCols == 1) && (rowComponent.numberOfFields > 0) &&
                    (getRowHeaderMaxColumnWidth(0) < rowComponent.preferredSize.width))) {
      fullColumnModel.getColumn(0).setPreferredWidth(rowComponent.preferredSize.width)
      rowHeaderColumnModel.getColumn(0).setPreferredWidth(rowComponent.preferredSize.width)
    } else {
      for ((count, fieldIndex) <- rowFieldHeadingCount.zipWithIndex) {
        val widths = new Array[Int](count)
        for (c <- 0 until count) {
          val max = getRowHeaderMaxColumnWidth(col)
          val preferredWidth = math.min(max + 5, PivotJTable.MaxColumnWidth)
          val fullColumn = fullColumnModel.getColumn(col)
          val rowHeaderColumn = rowHeaderColumnModel.getColumn(col)
          val widthToUse = preferredWidth
          fullColumn.setPreferredWidth(widthToUse)
          rowHeaderColumn.setPreferredWidth(widthToUse)
          widths(c) = preferredWidth
          col += 1
        }

        val totalWidth = widths.sum
        val fieldCompPrefWidth = rowComponent.guiField(fieldIndex).initialPreferredSize.width
        if (totalWidth < fieldCompPrefWidth) {
          // Need to add some space to each column.
          val perColDiff = ((fieldCompPrefWidth - totalWidth) / count)
          val startCol = col - count
          var newTotalWidth = 0
          for (c <- startCol until (startCol + count)) {
            val fullColumn = fullColumnModel.getColumn(c)
            val rowHeaderColumn = rowHeaderColumnModel.getColumn(c)
            val newWidth = widths(c - startCol) + perColDiff
            fullColumn.setPreferredWidth(newWidth)
            rowHeaderColumn.setPreferredWidth(newWidth)
            newTotalWidth += newWidth
          }
          if (newTotalWidth < fieldCompPrefWidth) {
            val extraWidth = fieldCompPrefWidth - newTotalWidth
            fullColumnModel.getColumn(0).setPreferredWidth(fullColumnModel.getColumn(0).getPreferredWidth + extraWidth)
            rowHeaderColumnModel.getColumn(0).setPreferredWidth(rowHeaderColumnModel.getColumn(0).getPreferredWidth + extraWidth)
          }
        } else {
          rowComponent.guiField(fieldIndex).setPreferredWidth(totalWidth)
        }
      }

      val firstAxisCell = rowHeaderTable.getValueAt(0,0).asInstanceOf[AxisCell]
      if (!(firstAxisCell.hidden || firstAxisCell.value.value == NullAxisValueType) || rowComponent.numberOfFields == 1) {
        val firstColWidth = fullColumnModel.getColumn(0).getPreferredWidth + 2
        fullColumnModel.getColumn(0).setPreferredWidth(firstColWidth)
        rowHeaderColumnModel.getColumn(0).setPreferredWidth(firstColWidth)
      }
    }
    val width = math.max(rowComponent.preferredSize.width, (0 until numCols).map(c => rowHeaderColumnModel.getColumn(c).getPreferredWidth).sum)
    val height = colHeaderRowCount0 * PivotJTable.RowHeight
    sizerPanel.preferredSize = new Dimension(width-1,10)
    rowComponent.preferredSize = new Dimension(width,height)

    val viewport = rowHeaderScrollPane.getViewport
    viewport.setPreferredSize(rowHeaderTable.getPreferredSize)
    viewport.setMinimumSize(new Dimension(rowHeaderTable.getPreferredSize.width, 20))
  }

  def reverse(mainTable:PivotJTable, rowHeaderTable:PivotJTable, colHeaderTable:PivotJTable) {
    // The order is important because we need to know whether to delete rows from the full table model.
    colHeaderTableModel.revert(colHeaderTable)
    val rowRowsRemoved = rowHeaderTableModel.revert(rowHeaderTable)
    val mainRowsRemoved = mainTableModel.revert(mainTable)
    if (rowRowsRemoved || mainRowsRemoved) {
      val r = fullTableModel.getRowCount - 1
      fullTableModel.fireTableRowsDeleted(r,r)
    }
  }

  def resizeColumnHeaderAndMainTableColumns(fullTable:JTable, mainTable:JTable, colHeaderTable:JTable,
                                            colHeaderScrollPane:JScrollPane, columnHeaderScrollPanePanel:Panel,
                                            mainTableScrollPane:JScrollPane) {
    val numRows = mainTable.getRowCount
    val colOffset = rowHeaderColCount0
    val numCols = mainColCount0
    val rowOffset = colHeaderRowCount0

    val tmpLabel = new JLabel("")

    val errorIconSize = 20
    val mainColumnWidths = for (col <- 0 until numCols) yield {
      var max = PivotJTable.MinColumnWidth
      for (row <- 0 until numRows) {
        val tableCell = mainTable.getValueAt(row, col).asInstanceOf[TableCell]
        tmpLabel.setFont(MainTableCellRenderer.selectFont(tableCell.value))
        val width = {
          tmpLabel.setText(tableCell.asString)
          if (tableCell.isError) {
            tmpLabel.getPreferredSize.width + errorIconSize
          } else {
            tmpLabel.getPreferredSize.width
          }
        }
        if (width > max) {
          max = width
        }
      }
      max
    }

    def cellWidth(axisCell:AxisCell, icon:ImageIcon):Int = {
      tmpLabel.setFont(MainTableCellRenderer.selectFont(axisCell.value.value.value))
      tmpLabel.setText(axisCell.text)
      tmpLabel.setIcon(icon)

      tmpLabel.getPreferredSize.width
    }

    val maxColumnWidths = {
      val previousRow = new Array[Int](numCols)
      for (row <- rowOffset-1 to 0 by -1) {
        var col = 0
        while (col < numCols) {
          val axisCell = colHeaderTable.getValueAt(row, col).asInstanceOf[AxisCell]
          if (axisCell == null) throw new Exception("No AxisCell found at " + (row, col) + " " + (colHeaderTable.getRowCount, colHeaderTable.getColumnCount) )
          if (row == rowOffset-1) {
            // I'm adding 5 here as the main table needs this for spacing. I need to figure out how I can calculate this better.
            previousRow(col) = math.max(mainColumnWidths(col), cellWidth(axisCell, null)) + 5
          } else {
            val icon = if (axisCell.collapsible.isDefined) {
              MainTableCellRenderer.PlusIcon
            } else {
              null
            }
            val width = cellWidth(axisCell, icon) + 5
            val span = axisCell.span.get
            val childWidth = (col until (col + span)).map(previousRow(_)).sum
            val missingWidth = math.max(0, width - childWidth)
            val cellPadding = {
              val cp = missingWidth / span
              if (cp == 0) cp else cp + 1
            }
            for (c <- col until (col + span)) {
              previousRow(c) += cellPadding
            }
          }
          col += axisCell.span.get
        }
      }
      previousRow.map(w => {
        math.min(w + 1, PivotJTable.MaxColumnWidth)
      })
    }

    val fullColumnModel = fullTable.getColumnModel
    val mainColumnModel = mainTable.getColumnModel
    val colHeaderColumnModel = colHeaderTable.getColumnModel

    (maxColumnWidths.zipWithIndex).foreach{case (width,colIndex) => {
      fullColumnModel.getColumn(colIndex + colOffset).setPreferredWidth(width)
      mainColumnModel.getColumn(colIndex).setPreferredWidth(width)
      colHeaderColumnModel.getColumn(colIndex).setPreferredWidth(width)
    }}

    val columnHeaderViewport = colHeaderScrollPane.getViewport
    val prefHeight = colHeaderTable.getPreferredSize.height
    columnHeaderViewport.setPreferredSize(new Dimension(colHeaderTable.getPreferredSize.width, prefHeight))
    columnHeaderViewport.setMinimumSize(new Dimension(PivotJTable.MinColumnWidth, prefHeight))
    columnHeaderScrollPanePanel.maximumSize = new Dimension(Integer.MAX_VALUE, prefHeight)
    mainTableScrollPane.getViewport.setPreferredSize(mainTable.getPreferredSize)
  }

  def tableAsString = {
    val ls = System.getProperty("line.separator")
    val sb = new StringBuilder
    var firstRow = true
    for (row <- 0 until fullTableModel.getRowCount) {
      var firstColumn = true
      if (firstRow) firstRow = false else sb.append(ls)
      for (column <- 0 until fullTableModel.getColumnCount) {
        if (firstColumn) firstColumn = false else sb.append("\t")
        sb.append(fullTableModel.getValueAt(row, column) match {
          case cell:TableCell if cell.asString.nonEmpty => cell.asString
          case axisCell:AxisCell if axisCell.text.nonEmpty => axisCell.text
          case _ => " "
        })
      }
    }
    sb.toString()
  }
}