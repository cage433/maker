package starling.pivot.view.swing

import fieldchoosers.RowComponent
import starling.quantity.UOM
import starling.pivot._
import javax.swing.table.AbstractTableModel
import java.awt.{Dimension, Graphics, Component => AWTComp, Color, KeyboardFocusManager}
import model.{NullAxisValueType, EditableInfo, ValueAxisValueType, AxisCell}
import collection.mutable.{ListBuffer, HashMap}
import scala.Array
import javax.swing._
import scala.swing.Swing._
import swing.{ScrollPane, ListView, Panel}
import swing.event.{KeyPressed, KeyTyped}

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
}

class PivotJTableModelHelper(var data0:Array[Array[TableCell]],
                     editableInfo:Option[EditableInfo],
                     var rowHeaderData0:Array[Array[AxisCell]],
                     var colHeaderData0:Array[Array[AxisCell]],
                     var uoms0:Array[UOM],
                     editedErrorOrAdded:(Boolean,Boolean,Boolean) => Unit,
                     resizeMainTableColumns: =>Unit,
                     resizeRowHeaderTableColumns: =>Unit,
                     fieldState:PivotFieldsState,
                     var extraFormatInfo:ExtraFormatInfo) {
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
    mainTableModel.updateNewCellsWithNewFormat()
    rowHeaderTableModel.updateNewCellsWithNewFormat()
    mainTableModel.fireTableRowsUpdated(0, mainTableModel.getRowCount-1)
    rowHeaderTableModel.fireTableRowsUpdated(0, rowHeaderTableModel.getRowCount-1)
    colHeaderTableModel.fireTableRowsUpdated(0, colHeaderTableModel.getRowCount-1)
    fullTableModel.fireTableRowsUpdated(0, fullTableModel.getRowCount-1)
    resizeRowHeaderTableColumns
    resizeMainTableColumns
  }

  def uoms = uoms0
  def uoms_=(u:Array[UOM]) = uoms0 = u

  private def clearAllEditsAndErrors = editedErrorOrAdded(false, false, false)

  private def updateAllEditsAndErrors {
    val rowHeaderValues = rowHeaderTableModel.overriddenValues
    val editsInRowHeader = rowHeaderValues.nonEmpty
    val errorsInRowHeader = rowHeaderValues.exists(_.state == EditableCellState.Error)

    val mainValues = mainTableModel.overriddenValues
    val editsInMain = mainValues.nonEmpty
    val errorsInMain = mainValues.exists(_.state == EditableCellState.Error)

    // Work out if any extra rows have been added.
    val rowHeaderAddedRows = rowHeaderTableModel.addedRows
    val mainAddedRows = mainTableModel.addedRows
    val addedRowsWithEdits = rowHeaderAddedRows.zipWithIndex.flatMap{case (row, rowIndex) => {
      if (row.exists(_.label.nonEmpty) || mainAddedRows(rowIndex).exists(_.text.nonEmpty)) {
        Some(row.toList ::: mainAddedRows(rowIndex).toList)
      } else {
        None
      }
    }}

    val addedRows = addedRowsWithEdits.nonEmpty

    // If any row isn't valid, we can't save
    def rowNotValid(row:List[Any]) = {
      row.exists(c => {c match {
        case ac:AxisCell => {
          if (ac.editable) ac.text.isEmpty else false
        }
        case tc:TableCell => {
          if (tc.editable) tc.text.isEmpty else false
        }
        case _ => true
      }})
    }
    val addedRowsErrors = addedRowsWithEdits.exists(rowNotValid)

    // Add another line if the last line has some text in it.
    val addAnotherLine = extraLine && (!rowHeaderAddedRows.last.forall(_.label.isEmpty) || !mainAddedRows.last.forall(_.text.isEmpty))
    if (addAnotherLine) {
      val numRows = rowHeaderTableModel.getRowCount
      rowHeaderTableModel.addAnotherRow
      mainTableModel.addAnotherRow
      rowHeaderTableModel.fireTableRowsInserted(numRows, numRows)
      mainTableModel.fireTableRowsInserted(numRows, numRows)
      val fullNumRows = numRows + colHeaderRowCount0
      fullTableModel.fireTableRowsInserted(fullNumRows, fullNumRows)
    } else {
      // If the bottom two rows are blank, delete the last one.
      if (rowHeaderAddedRows.size >= 2) {
        val deleteLastRow = rowHeaderAddedRows(rowHeaderAddedRows.size - 2).forall(_.label.isEmpty) &&
          mainAddedRows(mainAddedRows.size - 2).forall(_.text.isEmpty)

        if (deleteLastRow) {
          val numRows = rowHeaderTableModel.getRowCount - 1
          rowHeaderTableModel.removeLastRow
          mainTableModel.removeLastRow
          rowHeaderTableModel.fireTableRowsDeleted(numRows, numRows)
          mainTableModel.fireTableRowsDeleted(numRows, numRows)
          val fullNumRows = numRows + colHeaderRowCount0
          fullTableModel.fireTableRowsDeleted(fullNumRows, fullNumRows)
        }
      }
    }

    editedErrorOrAdded((editsInRowHeader | editsInMain), (errorsInRowHeader | errorsInMain | addedRowsErrors), addedRows)
  }

  val rowHeaderTableModel = new PivotJTableModel {
    private val overrideMap = new HashMap[(Int,Int),AxisCell]
    private val addedRows0 = new ListBuffer[Array[AxisCell]]
    private val blankCells = rowHeaderData0(0).map(_.copy(state = EditableCellState.Added, label = "", collapsible = None)).toList
    private def setupExtraRow {
      if (extraLine) {
        addedRows0 += blankCells.toArray
      }
    }
    setupExtraRow

    def reset {
      overrideMap.clear
      addedRows0.clear
      setupExtraRow
    }
    def overriddenValues = overriddenCells.values
    def overriddenCells = overrideMap.toMap
    def addedRows = addedRows0.toList
    def addAnotherRow = setupExtraRow
    def removeLastRow {addedRows0 -= addedRows0.last}

    def getRowCount = {rowHeaderData0.length + addedRows0.length}
    def getColumnCount = rowHeaderData0(0).length
    def getValueAt(rowIndex:Int, columnIndex:Int):AxisCell = {
      if (rowIndex < rowHeaderData0.length) {
        overrideMap.getOrElse((rowIndex,columnIndex), rowHeaderData0(rowIndex)(columnIndex))
      } else {
        addedRows0(rowIndex - rowHeaderData0.length)(columnIndex)
      }
    }
    def paintTable(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) = {
      PivotTableUI.rowHeaderPaintGrid(g, table, rMin, rMax, cMin, cMax, getColumnCount - 1)
      PivotTableUI.rowHeaderPaintCells(g, table, rendererPane, rMin, rMax, cMin, cMax)
    }
    def rowHeader(row:Int,col:Int) = true
    def mapCellToFields(row:Int, col:Int) = List()
    def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView) = {
      val path = (0 to col).map(colIndex => {getValueAt(row, colIndex).value}).toList
      pivotTableView.collapseOrExpandRow(path)
    }
    def deleteCells(cells:List[(Int,Int)]) = {
      cells.foreach{case (r,c) => if (r < rowHeaderData0.length) {
        overrideMap((r,c)) = getValueAt(r,c).copy(state = EditableCellState.Deleted)
      } else {
        addedRows0(r - rowHeaderData0.length)(c) = blankCells(c)
      }}
      updateTable(cells)
    }

    def resetCells(cells:List[(Int,Int)]) = {
      cells.foreach{case (r,c) => if (r < rowHeaderData0.length) {
        overrideMap -= ((r,c))
      } else {
        addedRows0(r - rowHeaderData0.length)(c) = blankCells(c)
      }}
      updateTable(cells)
    }
    
    def updateTable(cells:List[(Int,Int)]) {
      cells.foreach{case (r,c) => fireTableCellUpdated(r,c)}
      resizeRowHeaderTableColumns
      updateAllEditsAndErrors
    }

    override def isCellEditable(rowIndex:Int, columnIndex:Int) = getValueAt(rowIndex, columnIndex).editable
    override def setValueAt(value:AnyRef, rowIndex:Int, columnIndex:Int) = {
      val s = value.asInstanceOf[String].trim
      // Using a zero row index here as it doesn't really matter as long as it is the correct column.
      val rowHeaderField = rowHeaderData0(0)(columnIndex).value.field
      val parser = editableInfo.get.editableKeyFields(rowHeaderField)

      val rac = (rowIndex,columnIndex)
      val addedRow = rowIndex >= rowHeaderData0.length
      val (originalCell, currentCell) = if (addedRow) {
        (blankCells(columnIndex), getValueAt(rowIndex, columnIndex))
      } else {
        (rowHeaderData0(rowIndex)(columnIndex), getValueAt(rowIndex, columnIndex))
      }

      try {
        val (newValue,newLabel) = if (s.isEmpty) (Set.empty, "") else parser.parse(s)

        def updateEdited {
          val newValueAxisValue = currentCell.value.value.asInstanceOf[ValueAxisValueType].copy(value = newValue)
          val newCellValue = currentCell.value.copy(value = newValueAxisValue)
          val state = if (addedRow) EditableCellState.Added else (if (newLabel.trim.isEmpty) EditableCellState.Deleted else EditableCellState.Edited)
          if (addedRow) {
            addedRows0(rowIndex - rowHeaderData0.length)(columnIndex) = currentCell.copy(value = newCellValue, label = newLabel, state = state)
          } else {
            overrideMap(rac) = currentCell.copy(value = newCellValue, label = newLabel, state = state)
          }
          updateTable(List((rowIndex, columnIndex)))
        }

        if (currentCell.text == newLabel) {
          // Nothing has happened to it.
          // If this used to be an error we need to decide what to do with it.
          if (currentCell.state == EditableCellState.Error) {
            if (newLabel == originalCell.text) {
              // It is just the original value so reset it.
              resetCells(List((rowIndex, columnIndex)))
            } else {
              // It's gone from an error to an edited state.
              updateEdited
            }
          }
        } else if (originalCell.text != newLabel) {
          // It's been edited.
          updateEdited
        } else {
          // It's been reset.
          resetCells(List((rowIndex, columnIndex)))
        }
      } catch {
        case e:Exception => {
          // We are almost certainly here because of a parse exception.
          if (addedRow) {
            addedRows0(rowIndex - rowHeaderData0.length)(columnIndex) = currentCell.copy(label = s, state = EditableCellState.Error)
          } else {
            overrideMap(rac) = currentCell.copy(label = s, state = EditableCellState.Error)
          }
          updateTable(List((rowIndex, columnIndex)))
        }
      }
    }

    def updateNewCellsWithNewFormat() {
      val newValues = overrideMap.map{case (key, ac) => {
        val newAC = ac.value match {
          case pq:PivotQuantity => {
            ac.copy(label = PivotFormatter.formatPivotQuantity(pq, extraFormatInfo, false))
          }
          case other => ac
        }
        (key -> newAC)
      }}
      overrideMap.clear()
      overrideMap ++= newValues

      val newAddedRows = addedRows0.map(aca => {
        aca.map(ac => {
          ac.value match {
            case pq:PivotQuantity => {
              ac.copy(label = PivotFormatter.formatPivotQuantity(pq, extraFormatInfo, false))
            }
            case other => ac
          }
        })
      })
      addedRows0.clear()
      addedRows0 ++= newAddedRows
    }

    def textTyped(textField:JTextField, cellEditor:CellEditor, r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:AWTComp) {
      // Using a zero row index here as it doesn't really matter as long as it is the correct column.
      val rowHeaderField = rowHeaderData0(0)(c).value.field
      val parser = editableInfo.get.editableKeyFields(rowHeaderField)
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
    def getValueAt(rowIndex:Int, columnIndex:Int) = colHeaderData0(rowIndex)(columnIndex)
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
    def resetCells(cells:List[(Int,Int)]) {}
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
    def textTyped(textField:JTextField, cellEditor:CellEditor, r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:AWTComp) {}
    def finishedEditing() {popupMenu setVisible false}
    def popupShowing = popupMenu.isShowing
    def focusPopup() {
      KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu)
      popupListView.requestFocusInWindow()
      popupListView.selectIndices(0)
    }
  }

  val mainTableModel = new PivotJTableModel {
    private val overrideMap = new HashMap[(Int,Int),TableCell]
    private val addedRows0 = new ListBuffer[Array[TableCell]]
    private val blankCells = data0(0).map(_.copy(state = EditableCellState.Added, text = "")).toList
    private def setupExtraRow {
      if (extraLine) {
        addedRows0 += blankCells.toArray
      }
    }
    setupExtraRow

    def reset {
      overrideMap.clear()
      addedRows0.clear()
      setupExtraRow
    }
    def overriddenValues = overriddenCells.values
    def overriddenCells = overrideMap.toMap
    def addedRows = addedRows0.toList
    def addAnotherRow = setupExtraRow
    def removeLastRow {addedRows0 -= addedRows0.last}

    def getRowCount = data0.length + addedRows0.length
    def getColumnCount = data0(0).length
    def getValueAt(rowIndex:Int, columnIndex:Int):TableCell = {
      if (rowIndex < data0.length) {
        overrideMap.getOrElse((rowIndex,columnIndex), data0(rowIndex)(columnIndex))
      } else {
        addedRows0(rowIndex - data0.length)(columnIndex)
      }
    }
    override def isCellEditable(rowIndex:Int, columnIndex:Int) = getValueAt(rowIndex, columnIndex).editable
    def updateTable(cells:List[(Int,Int)]) {
      cells.foreach{case (r,c) => fireTableCellUpdated(r,c)}
      resizeMainTableColumns
      updateAllEditsAndErrors
    }

    def resetCells(cells:List[(Int,Int)]) = {
      cells.foreach{case (r,c) => if (r < data0.length) {
        overrideMap -= ((r,c))
      } else {
        addedRows0(r - data0.length)(c) = blankCells(c)
      }}
      updateTable(cells)
    }

    def updateNewCellsWithNewFormat() {
      val newValues = overrideMap.map{case (key, tc) => {
        val newTC = tc.value match {
          case pq:PivotQuantity => {
            tc.copy(text = PivotFormatter.formatPivotQuantity(pq, extraFormatInfo, false))
          }
          case other => tc
        }
        (key -> newTC)
      }}
      overrideMap.clear()
      overrideMap ++= newValues

      val newAddedRows = addedRows0.map(tca => {
        tca.map(tc => {
          tc.value match {
            case pq:PivotQuantity => {
              tc.copy(text = PivotFormatter.formatPivotQuantity(pq, extraFormatInfo, false))
            }
            case other => tc
          }
        })
      })
      addedRows0.clear()
      addedRows0 ++= newAddedRows
    }

    override def setValueAt(value:AnyRef, rowIndex:Int, columnIndex:Int) {
      val s = value.asInstanceOf[String].trim
      val measureInfo = colHeaderData0.find(_(columnIndex).value.isMeasure).get(columnIndex)
      val parser = editableInfo.get.editableMeasures(measureInfo.value.field)
      val rac = (rowIndex,columnIndex)

      val addedRow = rowIndex >= rowHeaderData0.length
      val (originalCell, currentCell) = if (addedRow) {
        (blankCells(columnIndex), getValueAt(rowIndex, columnIndex))
      } else {
        (data0(rowIndex)(columnIndex), getValueAt(rowIndex, columnIndex))
      }

      try {
        val (newValue,newLabel) = if (s.isEmpty) (Set.empty, "") else {
          val (v,l) = parser.parse(s)
          v match {
            case pq:PivotQuantity if uoms0.length > columnIndex => {
              val uom = uoms0(columnIndex)
              val dv = pq.doubleValue.get
              val newPQ = new PivotQuantity(dv, uom)
              (newPQ, PivotFormatter.formatPivotQuantity(newPQ, extraFormatInfo, false))
            }
            case _ => (v,l)
          }
        }

        def updateEdited() {
          val state = if (addedRow) EditableCellState.Added else (if (newLabel.trim.isEmpty) EditableCellState.Deleted else EditableCellState.Edited)
          if (addedRow) {
            addedRows0(rowIndex - data0.length)(columnIndex) = currentCell.copy(value = newValue, text = newLabel, state = state)
          } else {
            overrideMap(rac) = currentCell.copy(value = newValue, text = newLabel, state = state)
          }
          updateTable(List((rowIndex, columnIndex)))
        }

        if (currentCell.text == newLabel) {
          // Nothing has happened to it.
          // If this used to be an error we need to decide what to do with it.
          if (currentCell.state == EditableCellState.Error) {
            if (newLabel == originalCell.text) {
              // It is just the original value so reset it.
              resetCells(List((rowIndex, columnIndex)))
            } else {
              // It's gone from an error to an edited state.
              updateEdited()
            }
          }
        } else if (originalCell.text != newLabel) {
          // It's been edited.
          updateEdited()
        } else {
          // It's been reset.
          resetCells(List((rowIndex, columnIndex)))
        }
      } catch {
        case e:Exception => {
          // We are almost certainly here because of a parse exception.
          if (addedRow) {
            addedRows0(rowIndex - data0.length)(columnIndex) = currentCell.copy(text = s, state = EditableCellState.Error)
          } else {
            overrideMap(rac) = currentCell.copy(text = s, state = EditableCellState.Error)
          }
          updateTable(List((rowIndex, columnIndex)))
        }
      }
    }

    def paintTable(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) = {
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
    def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView) = {}
    def deleteCells(cells:List[(Int,Int)]) = {
      cells.foreach{case (r,c) => if (r < rowHeaderData0.length) {
        overrideMap((r,c)) = getValueAt(r,c).copy(state = EditableCellState.Deleted)
      } else {
        addedRows0(r - rowHeaderData0.length)(c) = blankCells(c)
      }}
      updateTable(cells)
    }
    def textTyped(textField:JTextField, cellEditor:CellEditor, r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:AWTComp) {
      val measureInfo = colHeaderData0.find(_(c).value.isMeasure).get(c)
      val parser = editableInfo.get.editableMeasures(measureInfo.value.field)
      val t = textField.getText.toLowerCase
      val vals = parser.acceptableValues
      if (vals.nonEmpty) {
        val filteredVals = vals.filter(_.toLowerCase.startsWith(t))
        if (filteredVals.nonEmpty) {
          val sortedVals = filteredVals.toList.sortWith(_.toLowerCase < _.toLowerCase)
          val currentListData = popupListView.listData.toList
          if (currentListData != sortedVals) {
            popupListView.listData = sortedVals
          }
          if (!popupMenu.isVisible) {
            popupMenu.setMinimumSize(viewScrollPane.preferredSize)
            popupMenu.show(textField, cellEditor, tableFrom, textField, 0, textField.getSize().height-1)
            focusOwner.map(_.requestFocusInWindow())
          }
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
    }
    listenTo(keys)
  }
  private val viewScrollPane = new ScrollPane(popupListView) {
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
  }
  popupMenu.add(viewScrollPane.peer)
  popupMenu.setBorder(EmptyBorder)

  private val allModels = List(rowHeaderTableModel, colHeaderTableModel, mainTableModel, blankAxisCellTableModel)

  val fullTableModel = new PivotJTableModel {
    def getRowCount = colHeaderTableModel.getRowCount + mainTableModel.getRowCount
    def getColumnCount = rowHeaderTableModel.getColumnCount + mainTableModel.getColumnCount

    def getValueAt(rowIndex:Int, columnIndex:Int) = {
      val (m,r,c) = getTableModel(rowIndex, columnIndex)
      m.getValueAt(r,c)
    }
    override def isCellEditable(rowIndex:Int, columnIndex:Int) = {
      val (m,r,c) = getTableModel(rowIndex, columnIndex)
      m.isCellEditable(r,c)
    }
    override def setValueAt(aValue:AnyRef, rowIndex:Int, columnIndex:Int) = {
      val (m,r,c) = getTableModel(rowIndex, columnIndex)
      m.setValueAt(aValue,r,c)
      fireTableCellUpdated(rowIndex,columnIndex)
    }
    def deleteCells(cells:List[(Int,Int)]) = {
      val modelMap = getTableModels(cells)
      modelMap.foreach{case (mod, list) => {
        mod.deleteCells(list.map{case (m,r,c) => (r,c)})
      }}
      cells.foreach{case (r,c) => {
        if (r < getRowCount) fireTableCellUpdated(r,c)
      }}
    }
    def resetCells(cells:List[(Int,Int)]) = {
      val modelMap = getTableModels(cells)
      modelMap.foreach{case (mod, list) => {
        mod.resetCells(list.map{case (m,r,c) => (r,c)})
      }}
      cells.foreach{case (r,c) => {
        if (r < getRowCount) fireTableCellUpdated(r,c)
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

    def paintTable(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) = {
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
    def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView) = {
      if (col < rowHeaderColCount0) {
        rowHeaderTableModel.collapseOrExpand(row - colHeaderRowCount0, col, pivotTableView)
      } else {
        colHeaderTableModel.collapseOrExpand(row, col - rowHeaderColCount0, pivotTableView)
      }
    }

    override def fireTableStructureChanged = {
      super.fireTableStructureChanged
      allModels.foreach(_.fireTableStructureChanged)
    }

    override def fireTableDataChanged = {
      super.fireTableDataChanged
      allModels.foreach(_.fireTableDataChanged)
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

  def pivotEdits = {
    val filterFieldToValues = Map() ++ fieldState.filterAreaFields.flatMap(f => {
      val (field, selection) = fieldState.filters.find{case (f0,sel) => f == f0}.get
      selection match {
        case SomeSelection(v) if v.size == 1 => Some((field -> v.head))
        case _ => None
      }
    })

    val rowHeaderEdits = rowHeaderTableModel.overriddenCells

    val rowHeaderPivotEdits = rowHeaderEdits.flatMap{case ((row,col), ac) => {
      val originalRowHeaderCells = rowHeaderData0(row)
      val originalRowHeaderFieldToValues = Map() ++ originalRowHeaderCells.map(cell => (cell.value.field -> cell.value.value.value))

      val editedRowHeaderCells = (0 until rowHeaderTableModel.getColumnCount).map(c => rowHeaderTableModel.getValueAt(row, c))
      val editedRowHeaderFieldToValues = Map() ++ editedRowHeaderCells.map(cell => (cell.value.field -> cell.value.value.value))

      val colHeaderDeleteFieldToValues = (0 until colHeaderTableModel.getColumnCount).flatMap(c => {
        val mainTableCell = mainTableModel.getValueAt(row, c)
        if ((mainTableCell == TableCell.Null) || mainTableCell.text == "") {
          None
        } else {
          val verticalSlice = (0 until colHeaderTableModel.getRowCount).map(r => colHeaderTableModel.getValueAt(r,c))
          if (verticalSlice.exists(ac => ac.value.isOtherValue | ac.value.isTotal)) {
            None
          } else {
            Some(Map() ++ verticalSlice.flatMap(cell => {
              if (cell.value.isMeasure | cell.value.field == Field.NullField) {
                None
              } else {
                Some((cell.value.field -> cell.value.value.value))
              }
            }))
          }
        }
      })
      val deletes = colHeaderDeleteFieldToValues.map(colMap => {
        DeletePivotEdit(colMap ++ originalRowHeaderFieldToValues ++ filterFieldToValues)
      })

      val colHeaderAmendFieldToValues = (0 until colHeaderTableModel.getColumnCount).flatMap(c => {
        val mainTableCell = mainTableModel.getValueAt(row, c)
        if ((mainTableCell == TableCell.Null) || (mainTableCell.text == "") ||
                (mainTableCell.state == EditableCellState.Deleted) || ac.state == EditableCellState.Deleted) {
          None
        } else {
          val verticalSlice = (0 until colHeaderTableModel.getRowCount).map(r => colHeaderTableModel.getValueAt(r,c))
          if (verticalSlice.exists(ac => ac.value.isOtherValue | ac.value.isTotal)) None else {
            Some(Map() ++ verticalSlice.flatMap(cell => {
              if (cell.value.field == Field.NullField) {
                None
              } else  if (cell.value.isMeasure) {
                Some((cell.value.field -> mainTableCell.value))
              } else {
                Some((cell.value.field -> cell.value.value.value))
              }
            }))
          }
        }
      })
      val amends = colHeaderAmendFieldToValues.map(colMap => {
        AmendPivotEdit(colMap ++ editedRowHeaderFieldToValues ++ filterFieldToValues)
      })

      deletes.toList ::: amends.toList
    }}

    val mainEdits = mainTableModel.overriddenCells
    val rowsThatHaveAlreadyBeenEdited = rowHeaderEdits.map{case ((row,_),_) => row}.toSet
    val mainPivotEdits = mainEdits.flatMap{case ((row,col), tc) => {
      if (rowsThatHaveAlreadyBeenEdited contains row) {
        None
      } else {
        val rowHeaderCells = rowHeaderData0(row)
        val rowHeaderFieldToValues = Map() ++ rowHeaderCells.map(cell => (cell.value.field -> cell.value.value.value))
        val measureInfo = colHeaderData0.find(_(col).value.isMeasure).get(col)
        val colHeaderFieldToValues = Map() ++ colHeaderData0.map(r => {
          val cell = r(col)
          (cell.value.field -> cell.value.value.value)
        }).filterNot{case (field,_) => (measureInfo.value.field == field) || (field == Field.NullField)}

        if (tc.state != EditableCellState.Deleted) {
          Some(AmendPivotEdit(rowHeaderFieldToValues ++ colHeaderFieldToValues ++ filterFieldToValues + (measureInfo.value.field -> tc.value)))
        } else {
          Some(DeletePivotEdit(rowHeaderFieldToValues ++ colHeaderFieldToValues ++ filterFieldToValues))
        }
      }
    }}

    // Get the added rows.
    val rowHeaderAddedRows = rowHeaderTableModel.addedRows
    val mainAddedRows = mainTableModel.addedRows
    val addedRows = rowHeaderAddedRows.zipWithIndex.flatMap{case (rowHeaderRow, rowIndex) => {
      if (rowHeaderRow.exists(_.label.isEmpty) || mainAddedRows(rowIndex).exists(_.text.isEmpty)) {
        None
      } else {
        val rowHeaderMap = Map() ++ rowHeaderRow.map(c => {c.value.field -> c.value.value.value})
        val mainMap = Map() ++ mainAddedRows(rowIndex).zipWithIndex.flatMap{case (tc, colIndex) => {
          val measureInfo = colHeaderData0.find(_(colIndex).value.isMeasure).get(colIndex)
          val colHeaderFieldToValues = Map() ++ colHeaderData0.map(r => {
            val cell = r(colIndex)
            (cell.value.field -> cell.value.value.value)
          }).filterNot{case (field,_) => (measureInfo.value.field == field) || (field == Field.NullField)}
          colHeaderFieldToValues + (measureInfo.value.field -> tc.value)
        }}
        Some(AmendPivotEdit(rowHeaderMap ++ mainMap ++ filterFieldToValues))
      }
    }}

    Set[PivotEdit]() ++ rowHeaderPivotEdits.toSet ++ mainPivotEdits.toSet ++ addedRows.toSet
  }
  def resetEdits = {
    mainTableModel.reset
    rowHeaderTableModel.reset
    fullTableModel.fireTableDataChanged
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

  def resizeColumnHeaderAndMainTableColumns(fullTable:JTable, mainTable:JTable, colHeaderTable:JTable,
                                            colHeaderScrollPane:JScrollPane, columnHeaderScrollPanePanel:Panel,
                                            mainTableScrollPane:JScrollPane) {
    val numRows = mainTable.getRowCount
    val colOffset = rowHeaderColCount0
    val numCols = mainColCount0
    val endCol = colOffset + numCols
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
    sb.toString
  }
}