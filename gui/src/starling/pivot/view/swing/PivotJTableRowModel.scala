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
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._


import PivotTableType._

class PivotJTableRowModel(helper: PivotJTableModelHelper, var rowHeaderData0:Array[Array[AxisCell]]) extends PivotJTableModel {
  val extraLine = helper.extraLine
  val fieldState = helper.fieldState
  val keyFields = helper.keyFields
  val pivotEdits = helper.pivotEdits
  val updateEdits = helper.updateEdits
  val editableInfo = helper.editableInfo
  val formatInfo = helper.formatInfo
  val extraFormatInfo = helper.extraFormatInfo
  val popupListView = helper.popupListView
  val popupMenu = helper.popupMenu

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

  def rowHeaderFieldToValues(rowIndex: Int, columnIndex: Option[Int] = None) = {
    val rowHeaderCells = rowHeaderData0(rowIndex) |> (row => columnIndex.fold(ci => row.take(ci + 1), row))

    rowHeaderCells.map(_.selection).toMap
  }

  private def key(rowIndex:Int, columnIndex:Int): Map[Field, SomeSelection] = {
    val filterFieldToValues = fieldState.singleValueFilterAreaFilters()
    val rowFilters = fieldState.rowFilters
    val columnFilters = fieldState.columnFilters

    (rowHeaderFieldToValues(rowIndex, Some(columnIndex)) ++ filterFieldToValues ++ rowFilters ++ columnFilters) & keyFields
  }

  override def deleteCells(cells:List[(Int,Int)], fireChange:Boolean) = {
    var editsToUse = pivotEdits
    val rightHandCellForEachRow = cells.toMultiMap.mapValues(_.max)
    rightHandCellForEachRow.foreach{case (r,c) => {
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
          editsToUse = editsToUse.withDelete(KeyFilter(key(r, c)))
        } else {
          overrideMap((r,c)) = OverrideDetails("", Added)
          val rowIndex = value.value.childKey.value.asInstanceOf[NewRowValue].rowIndex
          editsToUse = editsToUse.withNewAmended(rowIndex, value.value.field, None)
        }
      }
    }}
    if (fireChange && editsToUse != pivotEdits) {
      editsToUse = removeAddedRowsIfBlank(editsToUse)
      updateEdits(editsToUse, RowHeader)
    }
    editsToUse
  }

  private def removeAddedRowsIfBlank(edits:PivotEdits):PivotEdits = {
    //fullTableModel.removeAddedRowsIfBlank(edits)
    edits
  }

  override def resetCells(cells:List[(Int,Int)], fireChange:Boolean) = {
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
    if (fireChange && edits != pivotEdits) {
      updateEdits(edits, RowHeader)
    }
    edits
  }

  override def isCellEditable(rowIndex:Int, columnIndex:Int) = {
    val v = getValueAt(rowIndex, columnIndex)
    v.editable && v.shown && (v.state != EditableCellState.Deleted)
  }

  override def parser(row:Int, col:Int) = {
    val rowHeaderField = rowHeaderData0(0)(col).value.field
    editableInfo.get.fieldToParser(rowHeaderField)
  }

  override def setValuesAt(values:List[TableValue], currentEdits:Option[PivotEdits], fireChange:Boolean) = {
    var newPivotEdits = currentEdits match {
      case Some(e) => e
      case None => pivotEdits
    }
    values.foreach(tv => {
      val rowIndex = tv.row
      val columnIndex = tv.column
      val value = tv.value

      val s = value.asInstanceOf[String].trim
      // Using a zero row index here as it doesn't really matter as long as it is the correct column.
      val rowHeaderField = rowHeaderData0(0)(columnIndex).value.field
      val pars = parser(rowIndex, columnIndex)

      val (newValue,newLabel) = if (s.isEmpty) (None, "") else {
        val (v,t) = pars.parse(s, extraFormatInfo)
        (Some(v), t)
      }

      if (rowIndex < rowHeaderData0.length) {
        val currentCell = getValueAt(rowIndex, columnIndex)

        val originalLabel = currentCell.value.value.originalValue match {
          case None => "sfkjfhxcjkvuivyruvhrzzasaf$%£$££"
          case Some(origVal) => formatInfo.fieldToFormatter(currentCell.value.field).format(origVal, extraFormatInfo).text
        }

        if (originalLabel == newLabel) {
          newPivotEdits = resetCells(List((rowIndex, columnIndex)), false)
        } else if (currentCell.text != newLabel) {
          val stateToOverride = if (currentCell.state == EditableCellState.Added) EditableCellState.Added else EditableCellState.Edited
          overrideMap((rowIndex, columnIndex)) = OverrideDetails(newLabel, stateToOverride)
          currentCell.value.childKey.value match {
            case NewRowValue(ri) => newPivotEdits = newPivotEdits.withNewAmended(ri, rowHeaderField, newValue)
            case _ => newPivotEdits = newPivotEdits.withAmend(KeyFilter(key(rowIndex, columnIndex)), rowHeaderField, newValue)
          }
        }
      } else {
        overrideMap((rowIndex, columnIndex)) = OverrideDetails(newLabel, EditableCellState.Added)
        newValue.foreach { nv => {
          val row = initializedBlankRow + (rowHeaderField -> nv)
          helper.addRowToTables()
          newPivotEdits = newPivotEdits.withAddedRow(row)
        } }
      }
    })
    if (fireChange && newPivotEdits != pivotEdits) {
      updateEdits(newPivotEdits, RowHeader)
    }
    newPivotEdits
  }

  private def initializedBlankRow() = {
    (Map() ++ (keyFields.map(f => {f → UndefinedValue}))) ++ fieldState.singleValueFilterAreaFilters.mapValues(_.values.iterator.next)
  }

  override def setValueAt(value:AnyRef, rowIndex:Int, columnIndex:Int) {
    setValuesAt(List(TableValue(value, rowIndex, columnIndex)), Some(pivotEdits), true)
  }

  def acceptableValues(r:Int, c:Int) = {
    val rowHeaderField = rowHeaderData0(0)(c).value.field
    val parser = editableInfo.get.fieldToParser(rowHeaderField)
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

        popupMenu.setMinimumSize(helper.viewScrollPane.preferredSize)
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
