package starling.pivot.view.swing

import fieldchoosers.RowComponent
import starling.quantity.UOM
import starling.pivot._
import controller.{TreePivotFilter, PivotTable}
import javax.swing.table.AbstractTableModel
import java.awt.{Dimension, Graphics, Component => AWTComp, Color, KeyboardFocusManager}
import collection.mutable.{ListBuffer, HashMap}
import model._
import scala.Array
import javax.swing._
import scala.swing.Swing._
import swing.{ScrollPane, ListView, Panel}
import starling.pivot.EditableCellState._
import scala.collection.mutable.{HashMap => MMap}
import collection.immutable.Map
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._
import swing.event.{Event, MouseClicked, KeyPressed, KeyTyped}
import java.awt.event.{ActionEvent, KeyEvent}

case class OverrideDetails(text:String, state:EditableCellState)
case class TableValue(value:AnyRef, row:Int, column:Int)

object PivotTableType extends Enumeration {
  type PivotTableType = Value
  val RowHeader, ColumnHeader, Main, TopTable, BottomTable = Value
}

import PivotTableType._

abstract class PivotJTableModel extends AbstractTableModel {
  def paintTable(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int)
  def rowHeader(row:Int, col:Int):Boolean
  def mapCellToFieldsForMainTable(row:Int, col:Int):List[(Field, Selection)] = Nil
  def rowHeaderStrategySelection(row:Int, col:Int):Option[TreeFieldInformation] = None
  def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView)
  def deleteCells(cells:List[(Int,Int)], currentEdits:PivotEdits, fireChange:Boolean):PivotEdits = PivotEdits.Null
  def resetCells(cells:List[(Int,Int)], currentEdits:PivotEdits, fireChange:Boolean):PivotEdits = PivotEdits.Null
  def textTyped(textField:JTextField, cellEditor:CellEditor , r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:PivotJTable)
  def finishedEditing()
  def popupShowing:Boolean
  def selectPopupValueIfOnlyOneShowing(row:Int, col:Int) {}
  def singlePopupValue(row:Int, col:Int):Option[String] = None
  def focusPopup()
  def parser(row:Int, col:Int):PivotParser = TextPivotParser
  def setValuesAt(values:List[TableValue], currentEdits:PivotEdits, fireChange:Boolean):PivotEdits = PivotEdits.Null


  type CellType

  protected var overrideMap = new MMap[(Int,Int),CellType]()
  def revert(table:PivotJTable):Boolean = {
    false
  }
  def numberOfInPageEdits:Int = overrideMap.size
}

class PivotJTableModelHelper(var data0:Array[Array[TableCell]],
                     val pivotTable:PivotTable,
                     val rowHeaderData0X:Array[Array[AxisCell]],
                     var colHeaderData0:Array[Array[AxisCell]],
                     uoms0:Array[UOM],
                     resizeMainTableColumns: =>Unit,
                     resizeRowHeaderTableColumns: =>Unit,
                     val fieldState:PivotFieldsState,
                     var extraFormatInfo:ExtraFormatInfo,
                     val pagePivotEdits:PivotEdits,
                     val updateEdits:(PivotEdits, PivotTableType) => Unit,
                     updateNumberOfEditsAndState:(Int,Boolean) => Unit,
                     revalidateTheWholeThing: =>Unit) {

  val editableInfo:Option[EditableInfo] = pivotTable.editableInfo
  val fieldInfo:FieldInfo = pivotTable.fieldInfo
  val keyFields = editableInfo.map(_.keyFields).getOrElse(Set())
  val measureFields = editableInfo.map(_.measureFields).getOrElse(Set())
  val extraLine = editableInfo.fold(_.extraLine, false)

  def tellMainTableAboutDeletedRows(rows:List[Int]) {
    mainTableModel.rowsHaveBeenDeletedByTheRowHeaderTable(rows)
  }

  def tellRowHeaderTableAboutDeletedRows(rows:List[Int]) {
    rowHeaderTableModel.rowsHaveBeenDeletedByTheMainTable(rows)
  }

  def tellMainTableAboutDeletedRowsThatHaveBeenReset(rows:List[Int]) {
    mainTableModel.deletedRowsHaveBeenReset(rows)
  }

  def tellRowHeaderTableAboutDeletedRowsThatHaveBeenReset(rows:List[Int]) {
    rowHeaderTableModel.deletedRowsHaveBeenReset(rows)
  }

  def deleteEditedRowsIfRequired(edits:PivotEdits):PivotEdits = {
    var updatedEdits = edits
    val rowHeaderDeletes = rowHeaderTableModel.overrideEdits.filter{case (_,ac) => ac.state == Deleted}
    val mainDeletes = mainTableModel.overrideEdits.filter{case (_,tc) => tc.state == Deleted}
    val rowHeaderCellsToUpdate = rowHeaderDeletes.flatMap{case ((r,c0),ac) => {
      ac.value.value.originalValue match {
        case Some(v) => Some((r,c0))
        case None => None
      }
    }}.toList
    val mainCellsToUpdate = mainDeletes.flatMap{case ((r,c0),tc) => {
      tc.originalValue match {
        case Some(v) => Some((r,c0))
        case None => None
      }
    }}.toList

    updatedEdits = rowHeaderTableModel.resetCells(rowHeaderCellsToUpdate, updatedEdits, false)
    updatedEdits = mainTableModel.resetCells(mainCellsToUpdate, updatedEdits, false)

    val rowsToDelete = (rowHeaderCellsToUpdate ::: mainCellsToUpdate).map(_._1).distinct.sorted
    val c = rowHeaderTableModel.getColumnCount - 1
    rowsToDelete.foreach(r => {
      updatedEdits = updatedEdits.withDelete(KeyFilter(rowHeaderTableModel.key(r,c)))
    })

    updatedEdits
  }

  def removeAddedRowsIfBlank(edits:PivotEdits):PivotEdits = {

    val totalRows = bottomTableModel.getRowCount
    val totalColumns = bottomTableModel.getColumnCount

    (0 until totalRows).find(r => {
      bottomTableModel.getValueAt(r, 0) match {
        case ac:AxisCell => {
          ac.state == Added || ac.value.childKey.value.isInstanceOf[NewRowValue]
        }
      }
    }) match {
      case None => edits
      case Some(startRow) => {
        val endRow = if (extraLine) totalRows - 1 else totalRows
        def removeRow(r:Int) = {
          (0 until totalColumns).map(c => {
            bottomTableModel.getValueAt(r, c) match {
              case ac:AxisCell => ac.text
              case tc:TableCell => tc.text
            }
          }).forall(_.isEmpty)
        }
        val newRowsToRemove = (startRow until endRow).zipWithIndex.flatMap{case (r, i) => {
          if (removeRow(r)) {
            Some(i)
          } else {
            None
          }
        }}
        edits.removeNewRows(newRowsToRemove.toList)
      }
    }
  }

  def numColumnHeaderRows = colHeaderTableModel.getRowCount

  def tableUpdated() {
    val finalRowNumber = rowHeaderTableModel.getRowCount - 1
    val numRowHeaderColumns = rowHeaderTableModel.getColumnCount
    val numMainColumns = mainTableModel.getColumnCount

    if (extraLine) {
      val rowMaxRow = {
        val rs = rowHeaderTableModel.overrideEdits.map(_._1._1)
        if (rs.nonEmpty) {
          rs.max
        } else {
          0
        }
      }
      val mainMaxRow = {
        val rs = mainTableModel.overrideEdits.map(_._1._1)
        if (rs.nonEmpty) {
          rs.max
        } else {
          0
        }
      }

      val maxNumberOfRows = math.max(rowMaxRow, mainMaxRow)

      val rowsToAdd = ((finalRowNumber to maxNumberOfRows).flatMap(r => {
        val addRow = (0 until numRowHeaderColumns).exists(c => rowHeaderTableModel.getValueAt(r, c).state != AddedBlank) ||
          (0 until numMainColumns).exists(c => mainTableModel.getValueAt(r, c).state != AddedBlank)
        if (addRow) {
          Some(true)
        } else {
          None
        }
      })).size

      (0 until rowsToAdd).foreach(r => {
        addRowToTables()
      })
    }

    val rowsToDelete = (rowHeaderData0.length until finalRowNumber).filter(r => {
      (0 until numRowHeaderColumns).forall(c => rowHeaderTableModel.getValueAt(r, c).label.isEmpty) &&
        (0 until numMainColumns).forall(c => mainTableModel.getValueAt(r, c).text.isEmpty)
    })
    deleteRowsFromTables(rowsToDelete.toList)

    val numberOfEdits = rowHeaderTableModel.numberOfInPageEdits + mainTableModel.numberOfInPageEdits
    val validState = rowHeaderTableModel.validState && mainTableModel.validState
    updateNumberOfEditsAndState(numberOfEdits, validState)

    resizeMainTableColumns
    resizeRowHeaderTableColumns
    revalidateTheWholeThing
  }

  def allEdits(e:PivotEdits):PivotEdits = bottomTableModel.edits(e)

  val rowHeaderTableModel = new PivotJTableRowModel(this, rowHeaderData0X, extraFormatInfo)

  private var mainColCount0 = data0(0).length
  private var rowHeaderColCount0 = rowHeaderData0(0).length
  private var colHeaderRowCount0 = colHeaderData0.length

  def rowHeaderData0 = rowHeaderTableModel.rowHeaderData0

  def setData(rd:Array[Array[AxisCell]], cd:Array[Array[AxisCell]], d:Array[Array[TableCell]], extraFormatInfo:ExtraFormatInfo) {
    this.extraFormatInfo = extraFormatInfo
    rowHeaderTableModel.extraFormatInfo = extraFormatInfo
    data0 = d
    rowHeaderTableModel.rowHeaderData0 = rd
    colHeaderData0 = cd
    mainTableModel.fireTableRowsUpdated(0, mainTableModel.getRowCount-1)
    rowHeaderTableModel.fireTableRowsUpdated(0, rowHeaderTableModel.getRowCount-1)
    colHeaderTableModel.fireTableRowsUpdated(0, colHeaderTableModel.getRowCount-1)
    topTableModel.fireTableRowsUpdated(0, topTableModel.getRowCount-1)
    bottomTableModel.fireTableRowsUpdated(0, bottomTableModel.getRowCount-1)

    // TODO - update the edited cells in override map

    resizeMainTableColumns
    resizeRowHeaderTableColumns
  }

  def createSingleSelectionForKeyFields(): Map[Field, SomeSelection] = Map() ++ (keyFields -- fieldState.rowFields.toSet).flatMap(f => {
    val possibleValues = pivotTable.possibleValues
    fieldState.filters.find{case (f0,sel) => f == f0} match {
      case Some((field, selection)) => {
        selection match {
          case s@SomeSelection(v) if v.size == 1 => Some((field → s))
          case AllSelection => Some((field -> SomeSelection(Set(possibleValues(field).singleValue))))
          case _ => None
        }
      }
      case None => {Some( f -> SomeSelection(Set(possibleValues(f).singleValue)) )}
    }
  })

  def initializedBlankRow = Row(
    (Map() ++ (keyFields.map(f => {f → UndefinedValueNew}))) ++ createSingleSelectionForKeyFields().mapValues(_.values.iterator.next)
  )

  val colHeaderTableModel = new PivotJTableModel {
    type CellType = AxisCell

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
    def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView) {
      val path = (0 to row).map(rowIndex => {getValueAt(rowIndex, col).asInstanceOf[AxisCell].value}).toList
      pivotTableView.collapseOrExpandCol(path)
    }
    override def resetCells(cells:List[(Int,Int)], currentEdits:PivotEdits, fireChange:Boolean) = {
      var resetEdits = currentEdits
      cells.foreach{case (r,c) => {
        resetEdits = resetEdits.remove(getValueAt(r,c).value.pivotEdits)
      }}
      if (fireChange && (resetEdits != currentEdits)) {
        val newEdits = allEdits(resetEdits)
        updateEdits(newEdits, ColumnHeader)
      }
      resetEdits
    }
    def textTyped(textField:JTextField, cellEditor:CellEditor, r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:PivotJTable) {}
    def finishedEditing() {
      popupMenu setVisible false
      popupListView.peer.clearSelection()
    }
    def popupShowing = popupMenu.isShowing
    def focusPopup() {
      KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu)
      popupListView.requestFocusInWindow()
      popupListView.selectIndices(0)
    }
  }

  val blankAxisCellTableModel = new PivotJTableModel {
    type CellType = AxisCell

    private val values = fieldState.rowFields.zipWithIndex.flatMap{case (f,i) => {
      if (i < pivotTable.rowFieldHeadingCount.length) {
        val numRequired = pivotTable.rowFieldHeadingCount(i)
        Array.fill(numRequired)(AxisCell.Null.copy(label = f.name))
      } else {
        Array(AxisCell.Null)
      }
    }}

    def getRowCount = colHeaderData0.length
    def getColumnCount = rowHeaderData0(0).length
    def getValueAt(rowIndex:Int, columnIndex:Int) = {
      if (rowIndex == getRowCount - 1) {
        values(columnIndex)
      } else {
        AxisCell.Null
      }
    }
    def paintTable(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) {}
    def rowHeader(row:Int,col:Int) = false
    def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView) {}
    def textTyped(textField:JTextField, cellEditor:CellEditor, r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:PivotJTable) {}
    def finishedEditing() {
      popupMenu setVisible false
      popupListView.peer.clearSelection()
    }
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
    val bottom = bottomTableModel.getRowCount

    rowHeaderTableModel.addRow()
    mainTableModel.addRow()

    rowHeaderTableModel.fireTableRowsInserted(rowHeader, rowHeader)
    mainTableModel.fireTableRowsInserted(main, main)
    bottomTableModel.fireTableRowsInserted(bottom, bottom)
  }

  private def makeRanges(l:List[Int]):List[(Int,Int)] = {
    val groups = l.groupBy{i => (l.contains(i - 1), l.contains(i + 1))}.mapValues(_.sortWith(_<_))
    (groups.getOrElse((false, true), Nil).zip(groups.getOrElse((true, false), Nil)) ::: groups.getOrElse((false, false), Nil).map{i => (i, i)}).sortWith(_._1 < _._1)
  }

  private def deleteRowsFromTables(rows:List[Int]) {
    val reversedRows = rows.sorted.reverse
    rowHeaderTableModel.removeRows(reversedRows)
    mainTableModel.removeRows(reversedRows)

    val ranges = makeRanges(rows)
    ranges.foreach{case (start, end) => {
      rowHeaderTableModel.fireTableRowsDeleted(start, end)
      mainTableModel.fireTableRowsDeleted(start, end)
      bottomTableModel.fireTableRowsDeleted(start, end)
    }}
  }

  val mainTableModel = new PivotJTableModel {
    type CellType = TableCell

    private val addedRows0 = new ListBuffer[Array[TableCell]]
    private val blankCells = Array.fill(data0(0).length)(TableCell.BlankAddedCell).zipWithIndex.map{case (tc,c) => {
      colHeaderData0.find(_(c).value.isMeasure) match {
        case None => tc.copy(editable = false)
        case Some(cc) => {
          val f = cc(c).value.field
          if (measureFields.contains(f)) {
            tc
          } else {
            tc.copy(editable = false)
          }
        }
      }
    }}
    if (extraLine) {
      addedRows0 += blankCells
    }
    private val numOriginalRows = data0.length

    def addRow() {
      updateLastRowState()
      addedRows0 += blankCells
    }

    def rowsHaveBeenDeletedByTheRowHeaderTable(rows:List[Int]) {
      val cRange = (0 until getColumnCount)
      rows.foreach(r => {
        cRange.foreach(c => {
          val tc = getValueAt(r,c)
          overrideMap((r,c)) = tc.copy(state = Deleted)
        })
        fireTableRowsUpdated(r,r)
      })
    }

    def deletedRowsHaveBeenReset(rows:List[Int]) {
      val cRange = (0 until getColumnCount)
      rows.foreach(r => {
        cRange.foreach(c => {
          overrideMap -= ((r,c))
        })
        fireTableRowsUpdated(r,r)
      })
    }

    def removeRows(rows:List[Int]) {
      rows.foreach(r => {
        addedRows0.remove(r - numOriginalRows)
        overrideMap = overrideMap.filterNot{case ((r0,_),_) => r == r0}
        val keysToUpdate = overrideMap.filter{case ((r0,_),_) => (r0 > r)}
        overrideMap --= keysToUpdate.keySet
        overrideMap ++= keysToUpdate.map{case((r0,c0),v) => (r0-1,c0) -> v}
      })
    }

    private def updateLastRowState() {
      val r = getRowCount - 1
      for (c <- (0 until getColumnCount)) {
        val tc = getValueAt(r, c)
        val newState = if (tc.state == Error) {
          Error
        } else {
          Added
        }
        overrideMap((r,c)) = tc.copy(state = newState)
      }
    }

    def validState = !overrideMap.exists{case (_,tc) => tc.state == Error}

    def field(col:Int) = {
      colHeaderData0.find(_(col).value.isMeasure).get(col).value.field
    }

    def overrideEdits = overrideMap.toMap

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
    val getColumnCount = data0(0).length
    def getValueAt(rowIndex:Int, columnIndex:Int):TableCell = {
      def v = {
        if (rowIndex < numOriginalRows) {
          data0(rowIndex)(columnIndex)
        } else {
          addedRows0(0)(columnIndex)
        }
      }
      overrideMap.getOrElse((rowIndex, columnIndex), v)
    }
    override def isCellEditable(rowIndex:Int, columnIndex:Int) = {
      val v = getValueAt(rowIndex, columnIndex)
      v.editable && (v.state != EditableCellState.Deleted)
    }
    override def resetCells(cells:List[(Int,Int)], currentEdits:PivotEdits, fireChange:Boolean) = {
      var resetEdits = currentEdits
      val deletedRowsToReset = new ListBuffer[Int]
      cells.foreach{case (r,c) => {
        val k = (r,c)
        if (overrideMap.contains(k)) {
          if (overrideMap(k).state == Deleted) {
            deletedRowsToReset += r
          }
        }
        if (r >= numOriginalRows) {
          overrideMap(k) = overrideMap(k).copy(text = "", longText = Some(""), state = Added)
        } else {
          overrideMap -= k
        }
        resetEdits = resetEdits.remove(getValueAt(r,c).edits)
      }}
      deletedRowsToReset.distinct.foreach(r => {
        (0 until getColumnCount).foreach(c => {
          overrideMap -= ((r,c))
        })
      })

      tellRowHeaderTableAboutDeletedRowsThatHaveBeenReset(deletedRowsToReset.toList)

      maybeUpdate(fireChange, currentEdits, resetEdits)
      resetEdits
    }

    override def deleteCells(cells:List[(Int,Int)], currentEdits:PivotEdits, fireChange:Boolean) = {
      var deleteEdits = currentEdits

      def isAdded(r:Int, tc:TableCell):Boolean = {
        (tc.state == Added) || (r >= numOriginalRows) || rowHeaderTableModel.getValueAt(r,0).value.childKey.value.isInstanceOf[NewRowValue]
      }

      val (newRowCells, normalCells) = cells.partition{case (r,c) => isAdded(r, getValueAt(r,c))}

      newRowCells.foreach{case (r,c) => {
        val value = getValueAt(r,c)
        val rowCell = rowHeaderTableModel.getValueAt(r,0)
        rowCell.value.childKey.value match {
          case NewRowValue(rowIndex) => {
            deleteEdits = deleteEdits.withNewAmended(rowIndex, field(c), None)
            overrideMap((r,c)) = value.copy(text = "", longText = Some(""), state = Deleted)
          }
          case _ => {
            val k = ((r,c))
            overrideMap(k) = overrideMap(k).copy(text = "", longText = Some(""), state = Added)
          }
        }
      }}

      val cRange = (0 until getColumnCount)
      val normalRowsToDelete = normalCells.map(_._1).distinct
      normalRowsToDelete.foreach(r => {
        cRange.foreach(c => {
          overrideMap -= ((r,c))
          val tc = getValueAt(r,c)
          overrideMap((r,c)) = tc.copy(state = Deleted)
        })
      })

      tellRowHeaderTableAboutDeletedRows(normalRowsToDelete)

      if (fireChange) {
        deleteEdits = deleteEditedRowsIfRequired(deleteEdits)
        deleteEdits = removeAddedRowsIfBlank(deleteEdits)
      }

      maybeUpdate(fireChange, currentEdits, deleteEdits)
      deleteEdits
    }

    def key(rowIndex:Int, columnIndex:Int, field0:Field):Map[Field, SomeSelection] = {
      val rowHeaderFieldToValues = rowHeaderTableModel.rowHeaderFieldToValues(rowIndex)

      val colHeaderFieldToValues = colHeaderData0.map(_(columnIndex).selection).toMap
        .filterKeysNot{case field => (field0 == field) || (field == Field.NullField)}

      val filterFieldToValues = fieldState.filtersInTheFilterArea.collect{case (f, SomeSelection(v)) => (f, SomeSelection(v))}

      (rowHeaderFieldToValues ++ colHeaderFieldToValues ++ filterFieldToValues) //& keyFields
    }

    override def parser(row:Int, col:Int) = editableInfo.get.fieldToParser(field(col))

    override def setValuesAt(values:List[TableValue], currentEdits:PivotEdits, fireChange:Boolean) = {
      var anyResetEdits = currentEdits
      values.foreach(tv => {
        val r = tv.row
        val c = tv.column
        val sValue = tv.value.asInstanceOf[String].trim

        val currentValue = getValueAt(r,c)
        val currentText = currentValue.text.trim

        if (sValue != currentText && currentValue.editable) {

          val uom = uoms0(c)
          val (newValue,newLabel,stateToUse) =  try {

            val stringValueToUse = if (sValue.nonEmpty && sValue.last.isDigit) {
              if (uom != UOM.NULL) {
                sValue + " " + uom.asString
              } else if (currentText.nonEmpty && !currentText.last.isDigit) {
                val lastDigit = currentText.lastIndexWhere(_.isDigit) + 1
                val uomText = currentText.substring(lastDigit)
                sValue + " " + uomText
              } else {
                sValue
              }
            } else {
              sValue
            }

            val state = if (r < numOriginalRows && currentValue.state != Added) Edited else Added

            val pars = parser(r, c)
            val (v,t) = pars.parse(stringValueToUse, extraFormatInfo)

            (Some(v), t, state)
          } catch {
            case e:Exception => (None, sValue, Error)
          }

          if (Some(currentValue.value) != newValue) {
            val k = (r,c)
            overrideMap -= k
            val originalCell = getValueAt(r,c)
            val originalLabel = originalCell.originalValue match {
              case None => {
                if (originalCell.text.nonEmpty) {
                  originalCell.text
                } else {
                  "sfkjfhxcjkvuivyruvhrzzasaf$%£$££"
                }
              }
              case Some(origVal) => {
                fieldInfo.fieldToFormatter(field(c)).format(origVal, extraFormatInfo)
                fieldInfo.fieldToFormatter(field(c)).format(origVal, extraFormatInfo).text
              }
            }

            if (originalLabel == newLabel) {
              anyResetEdits = resetCells(List(k), anyResetEdits, false)
            } else {
              if (stateToUse == Error) {
                overrideMap(k) = originalCell.copy(text = sValue, longText = Some(sValue), state = Error)
              } else {
                val actualLabelToUse = if (newLabel.endsWith(uom.asString)) {
                  newLabel.replaceAll(uom.asString, "")
                } else {
                  newLabel
                }
                overrideMap(k) = originalCell.copy(text = actualLabelToUse, longText = Some(newLabel), state = stateToUse, value = newValue.get, textPosition = RightTextPosition)
              }
            }
          }
        }
      })
      maybeUpdate(fireChange, currentEdits, anyResetEdits)
      anyResetEdits
    }

    private def maybeUpdate(fireChange:Boolean, currentEdits:PivotEdits, otherEdits:PivotEdits) {
      if (fireChange) {
        if (otherEdits != currentEdits) {
          val newEdits = allEdits(otherEdits)
          updateEdits(newEdits, Main)
        } else {
          tableUpdated()
        }
      }
    }

    override def setValueAt(value:AnyRef, rowIndex:Int, columnIndex:Int) {
      val s = value.asInstanceOf[String].trim
      if (s.isEmpty) {
        val currentCell = getValueAt(rowIndex, columnIndex)
        if (currentCell.state != AddedBlank) {
          deleteCells(List((rowIndex, columnIndex)), pagePivotEdits, true)
        }
      } else {
        setValuesAt(List(TableValue(s, rowIndex, columnIndex)), pagePivotEdits, true)
      }
    }

    def paintTable(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) {
      PivotTableUI.mainPaintGrid(g,table,rMin,rMax,cMin,cMax)
      PivotTableUI.rowHeaderPaintCells(g,table,rendererPane,rMin,rMax,cMin,cMax)
    }
    def rowHeader(row:Int,col:Int) = false
    override def mapCellToFieldsForMainTable(row:Int, col:Int) = {
      def valueAxisValue(axisValue: AxisValue):Option[(Field, SomeSelection)] = axisValue.value partialMatch {
        case ValueAxisValueType(v) => axisValue.field → SomeSelection(Set(v))
      }

      val rowFilters = (0 until rowHeaderTableModel.getColumnCount).flatMap { i =>
        valueAxisValue(rowHeaderTableModel.getValueAt(row, i).value)
      }
      val columnFilters = (0 until colHeaderTableModel.getRowCount).flatMap { i =>
        valueAxisValue(colHeaderTableModel.getValueAt(i, col).value)
      }
      rowFilters.toMap.toList ::: columnFilters.toMap.toList
    }
    def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView) {}

    def acceptableValues(r:Int, c:Int) = {
      val measureInfo = colHeaderData0.find(_(c).value.isMeasure).get(c)
      val parser = editableInfo.get.fieldToParser(measureInfo.value.field)
      parser.acceptableValues
    }

    def textTyped(textField:JTextField, cellEditor:CellEditor, r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:PivotJTable) {
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
    def finishedEditing() {
      popupMenu setVisible false
      popupListView.peer.clearSelection()
    }
    def popupShowing = popupMenu.isShowing
    def focusPopup() {
      KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu)
      popupListView.requestFocusInWindow()
      popupListView.selectIndices(0)
    }
    override def selectPopupValueIfOnlyOneShowing(row:Int, col:Int) {
      if (popupShowing && popupListView.listData.size == 1) {
        val valueToUse = popupListView.listData.head
        setValueAt(valueToUse, row, col)
      }
    }

    override def singlePopupValue(row:Int, col:Int) = {
      if (popupShowing && popupListView.listData.size == 1) {
        val valueToUse = popupListView.listData.head
        Some(valueToUse)
      } else {
        None
      }
    }
  }

  val popupMenu = new JPopupMenu {
    var editor:JTextField = _
    var cellEditor:CellEditor = _
    var tableToFocus:PivotJTable = _

    def show(ed:JTextField, cEditor:CellEditor, tToFocus:PivotJTable, invoker:AWTComp, x:Int, y:Int) {
      editor = ed
      cellEditor = cEditor
      tableToFocus = tToFocus
      show(invoker, x, y)
    }
  }

  val popupListView = new ListView(List(" ")) {
    fixedCellWidth = 300
    selection.intervalMode = ListView.IntervalMode.Single
    peer.setFocusTraversalKeysEnabled(false)
    def selectText(t:String) {
      popupMenu.editor.setText(t)
      popupMenu.cellEditor.stopCellEditing()
    }
    reactions += {
      case KeyPressed(_,scala.swing.event.Key.Enter,_,_) => selectText(selection.items.head)
      case KeyPressed(_,scala.swing.event.Key.Tab,_,_) => {
        selectText(selection.items.head)
        val am = SwingUtilities.getUIActionMap(popupMenu.tableToFocus)
        val im = SwingUtilities.getUIInputMap(popupMenu.tableToFocus, JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)

        val tabBinding = im.get(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0))
        val tabAction = am.get(tabBinding)

        val ae = new ActionEvent(popupMenu.tableToFocus, 0, "")
        tabAction.actionPerformed(ae)
      }
      case e@KeyPressed(_,scala.swing.event.Key.BackSpace,_,_) => e.consume()
      case e@KeyPressed(_,scala.swing.event.Key.Escape,_,_) => {
        e.consume()
        selectIndices(-1)
        KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu.editor)
        onEDT({
          popupMenu.editor.requestFocusInWindow()
          onEDT(popupMenu.editor.setCaretPosition(popupMenu.editor.getText.length()))
        })
      }
      case KeyPressed(_,scala.swing.event.Key.Up,_,_) if selection.indices.head == 0 => {
        selectIndices(-1)
        KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu.editor)
        onEDT({
          popupMenu.editor.requestFocusInWindow()
          onEDT(popupMenu.editor.setCaretPosition(popupMenu.editor.getText.length()))
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
  val viewScrollPane = new ScrollPane(popupListView) {
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
  }
  popupMenu.add(viewScrollPane.peer)
  popupMenu.setBorder(EmptyBorder)

  val topTableModel = new PivotJTableModel {
    type CellType = AxisCell

    private val allModels = List(colHeaderTableModel, blankAxisCellTableModel)

    def getRowCount = colHeaderTableModel.getRowCount
    def getColumnCount = rowHeaderTableModel.getColumnCount + colHeaderTableModel.getColumnCount
    def getValueAt(rowIndex:Int, columnIndex:Int) = {
      val (m,c) = getTableModel(columnIndex)
      m.getValueAt(rowIndex, c)
    }
    override def parser(row:Int, col:Int) = {
      val (m,c) = getTableModel(col)
      m.parser(row,c)
    }

    def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView) {
      if (col >= rowHeaderTableModel.getColumnCount) {
        colHeaderTableModel.collapseOrExpand(row, col - rowHeaderTableModel.getColumnCount, pivotTableView)
      }
    }

    def paintTable(g:Graphics, table:JTable, rendererPane: CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) {
      // Paint the col header table.
      val cMinL = math.max(rowHeaderColCount0, cMin)
      val cMaxL = math.max(rowHeaderColCount0, cMax)

      PivotTableUI.colHeaderPaintGrid(g, table, rMin, rMax, cMinL, cMaxL)
      PivotTableUI.colHeaderPaintCells(g, table, rendererPane, rMin, rMax, cMinL, cMaxL)
    }

    private def getTableModels(cells:List[(Int,Int)]) = {
      cells.map{case (r,c) => {
        val (m, col) = getTableModel(c)
        (m, r, col)
      }}.groupBy(_._1)
    }

    override def resetCells(cells:List[(Int, Int)], currentEdits:PivotEdits, fireChange:Boolean) = {
      var editsToUse = currentEdits
      val modelMap = getTableModels(cells)
      modelMap.foreach{case (mod, list) => {
        val cellsForMod = list.map{case (_,r,c) => (r,c)}
        editsToUse = mod.resetCells(cellsForMod, editsToUse, false)
      }}
      if (fireChange) {
        if (editsToUse != currentEdits) {
          val newEdits = allEdits(editsToUse)
          updateEdits(newEdits, TopTable)
        } else {
          tableUpdated()
        }
      }
      editsToUse
    }

    def rowHeader(row:Int, col:Int) = false
    def finishedEditing() {
      popupMenu setVisible false
      popupListView.peer.clearSelection()
    }
    def textTyped(textField:JTextField, cellEditor:CellEditor, r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:PivotJTable) {}
    def popupShowing = popupMenu.isShowing
    def focusPopup() {
      KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu)
      popupListView.requestFocusInWindow()
      popupListView.selectIndices(0)
    }
    override def fireTableStructureChanged() {
      super.fireTableStructureChanged()
      allModels.foreach(_.fireTableStructureChanged())
    }
    override def fireTableDataChanged() {
      super.fireTableDataChanged()
      allModels.foreach(_.fireTableDataChanged())
    }
    private def getTableModel(columnIndex:Int) = {
      if (columnIndex < rowHeaderTableModel.getColumnCount) {
        (blankAxisCellTableModel, columnIndex)
      } else {
        (colHeaderTableModel, columnIndex - rowHeaderTableModel.getColumnCount)
      }
    }
  }

  val bottomTableModel = new PivotJTableModel {
    private val allModels = List(rowHeaderTableModel, mainTableModel)

    def getRowCount = rowHeaderTableModel.getRowCount
    def getColumnCount = rowHeaderTableModel.getColumnCount + mainTableModel.getColumnCount

    override def setValuesAt(values:List[TableValue], currentEdits:PivotEdits, fireChange:Boolean) = {
      var anyResetEdits = currentEdits
      val modelToData = values.map(tv => {
        val r = tv.row
        val c = tv.column
        val (mod, col) = getTableModel(c)
        (mod, r, col, tv.value)
      }).groupBy(_._1)

      modelToData.foreach{case (mod, list) =>  {
        val valsForMod = list.map{case (_,r,c,v) => TableValue(v, r, c)}
        anyResetEdits = mod.setValuesAt(valsForMod, anyResetEdits, false)
      }}

      if (fireChange) {
        if (anyResetEdits != currentEdits) {
          val newEdits = edits(anyResetEdits)
          updateEdits(newEdits, BottomTable)
        } else {
          tableUpdated()
        }
      }

      anyResetEdits
    }

    def getValueAt(rowIndex:Int, columnIndex:Int) = {
      val (m,c) = getTableModel(columnIndex)
      m.getValueAt(rowIndex, c)
    }

    private def getTableModels(cells:List[(Int,Int)]) = {
      cells.map{case (r,c) => {
        val (m, col) = getTableModel(c)
        (m, r, col)
      }}.groupBy(_._1)
    }

    override def resetCells(cells:List[(Int,Int)], currentEdits:PivotEdits, fireChange:Boolean) = {
      var editsToUse = currentEdits
      val modelMap = getTableModels(cells)
      modelMap.foreach{case (mod, list) => {
        val cellsForMod = list.map{case (_,r,c) => (r,c)}
        editsToUse = mod.resetCells(cellsForMod, editsToUse, false)
      }}
      if (fireChange) {
        if (editsToUse != currentEdits) {
          val newEdits = allEdits(editsToUse)
          updateEdits(newEdits, BottomTable)
        } else {
          tableUpdated()
        }
      }
      editsToUse
    }

    override def deleteCells(cells:List[(Int,Int)], currentEdits:PivotEdits, fireChange:Boolean) = {
      val modelMap = getTableModels(cells).multiMapValues(_.tail)
      var editsToUse = currentEdits
      modelMap.foreach{case (mod, cellsForMod) => {
        editsToUse = mod.deleteCells(cellsForMod, editsToUse, false)
      }}
      if (fireChange) {
        editsToUse = deleteEditedRowsIfRequired(editsToUse)
        editsToUse = removeAddedRowsIfBlank(editsToUse)
        if (editsToUse != pagePivotEdits) {
          val newEdits = allEdits(editsToUse)
          updateEdits(newEdits, BottomTable)
        } else {
          tableUpdated()
        }
      }
      editsToUse
    }

    def edits(e:PivotEdits) = {
      var updatedEdits = e
      var newRows = Map[Int,Row]()

      def rowAlreadyAdded(r:Int) = {rowHeaderTableModel.getValueAt (r, 0).value.childKey.value.isInstanceOf[NewRowValue]}

      val (rowErrors, rowEditsMap0) = rowHeaderTableModel.overrideEdits.partition{case (_,ac) => ac.state == Error}
      val (mainErrors, mainEditsMap0) = mainTableModel.overrideEdits.partition{case (_,tc) => tc.state == Error}

      val rowEditsByRow = rowEditsMap0.groupBy{case ((r,_),_) => r}
      val mainEditsByRow = mainEditsMap0.groupBy{case ((r,_),_) => r}

      val rowsWithErrors = rowErrors.map{case ((r,_),_) => r}.toSet ++ mainErrors.map{case ((r,_),_) => r}.toSet

      val rowsToRemove = rowsWithErrors.filter(r => {
        rowEditsByRow.getOrElse(r, Map()).forall{case (_,ac) => ac.label.isEmpty} &&
          mainEditsByRow.getOrElse(r, Map()).forall{case (_,tc) => tc.text.isEmpty}
      })

      val rowEditsMap = rowEditsMap0.filterNot{case ((r,_),_) => rowsToRemove.contains(r)}
      val mainEditsMap = mainEditsMap0.filterNot{case ((r,_),_) => rowsToRemove.contains(r)}

      val (rowDeleteEdits, rowOtherEdits) = rowEditsMap.partition{case (_,ac) => ac.state == Deleted}
      val (rowAdded, rowAmended) = rowOtherEdits.partition{case (_,ac) => ac.state == Added}

      rowAmended.foreach{case ((r,c), ac) => {
        updatedEdits = updatedEdits.withAmend(KeyFilter(rowHeaderTableModel.key(r,c)), rowHeaderTableModel.field(c), Some(ac.actualValue))
      }}

      val rowRowsToMaps = rowAdded.groupBy{case ((r,_),_) => r}.sorted
      rowRowsToMaps.foreach{case (r, cells) => {
        if (rowAlreadyAdded(r)) {
          cells.map{case ((r0,c0),ac) => {
            ac.value.childKey.value match {
              case NewRowValue(ri) => {
                updatedEdits = updatedEdits.withNewAmended(ri, rowHeaderTableModel.field(c0), Some(ac.actualValue))
              }
            }
          }}
        } else {
          if (cells.exists{case (_,ac) => ac.label.nonEmpty}) {
            val row = initializedBlankRow ++ cells.map{case ((r0,c0),ac) => rowHeaderTableModel.field(c0) -> ac.actualValue}.toMap
            newRows += ((r -> row))
          }
        }
      }}

      // --------------------------------------------------------------------

      val (mainDeleteEdits, mainOtherEdits) = mainEditsMap.partition{case (_,tc) => tc.state == Deleted}
      val (mainAdded, mainAmended) = mainOtherEdits.partition{case (_,tc) => tc.state == Added}
      mainAmended.foreach{case ((r,c), tc) => {
        tc.state match {
          case Edited => {
            val f = mainTableModel.field(c)
            updatedEdits = updatedEdits.withAmend(KeyFilter(mainTableModel.key(r, c, f)), f, Some(tc.value))
          }
        }
      }}

      val mainRowsToMaps = mainAdded.groupBy{case ((r,_),_) => r}.sorted
      mainRowsToMaps.foreach{case (r, cells) => {
        if (rowAlreadyAdded(r)) {
          cells.map{case ((r0,c0),tc) => {
            val rowCellForCheck = rowHeaderTableModel.getValueAt(r0, 0)
            rowCellForCheck.value.childKey.value match {
              case NewRowValue(ri) => {
                updatedEdits = updatedEdits.withNewAmended(ri, mainTableModel.field(c0), Some(tc.value))
              }
              case UndefinedValueNew =>
            }
          }}
        } else {
          val rowToUse = newRows.getOrElse(r, initializedBlankRow)
          val row = rowToUse ++ cells.filter{case (_,tc) => tc.text.nonEmpty}.map{case ((r0,c0),tc) => mainTableModel.field(c0) -> tc.value}.toMap
          newRows += ((r -> row))
        }
      }}

      val rowHeaderColumnCount = rowHeaderTableModel.getColumnCount - 1
      val deletesByRow = (rowDeleteEdits.map{case ((r,_),_) => r} ++ mainDeleteEdits.map{case ((r,_),_) => r}).filterNot(r => {
        val ac = rowHeaderTableModel.getValueAt(r, 0)
        (ac.state == Added) || ac.value.childKey.value.isInstanceOf[NewRowValue]
      }).toList.distinct.sorted

      deletesByRow.foreach{r => {
        updatedEdits = updatedEdits.withDelete(KeyFilter(rowHeaderTableModel.key(r, rowHeaderColumnCount)))
      }}

      newRows.sorted.foreach{case (_, row) => {
        updatedEdits = updatedEdits.withAddedRow(row)
      }}

      println("---")
      println("ALL EDITS")
      println(updatedEdits)
      println("---")

      updatedEdits
    }

    override def isCellEditable(rowIndex:Int, columnIndex:Int) = {
      val (m,c) = getTableModel(columnIndex)
      m.isCellEditable(rowIndex,c)
    }
    override def setValueAt(aValue:AnyRef, rowIndex:Int, columnIndex:Int) {
      val (m,c) = getTableModel(columnIndex)
      m.setValueAt(aValue,rowIndex,c)
    }
    override def parser(row:Int, col:Int) = {
      val (m,c) = getTableModel(col)
      m.parser(row,c)
    }

    override def mapCellToFieldsForMainTable(row:Int, col:Int) = {
      val offsetCol = col - rowHeaderTableModel.getColumnCount
      if (offsetCol >= 0) {
        mainTableModel.mapCellToFieldsForMainTable(row, offsetCol)
      } else {
        List()
      }
    }

    override def rowHeaderStrategySelection(row:Int, col:Int) = {
      if (col < rowHeaderTableModel.getColumnCount) {
        rowHeaderTableModel.rowHeaderStrategySelection(row, col)
      } else {
        None
      }
    }

    def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView) {
      if (col < rowHeaderTableModel.getColumnCount) {
        rowHeaderTableModel.collapseOrExpand(row, col, pivotTableView)
      }
    }
    def textTyped(textField:JTextField, cellEditor:CellEditor, r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:PivotJTable) {
      val (m,col) = getTableModel(c)
      m.textTyped(textField, cellEditor, r, col, focusOwner, tableFrom)
    }
    def rowHeader(row:Int, col:Int) = (col < rowHeaderTableModel.getColumnCount)
    def finishedEditing() {
      popupMenu setVisible false
      popupListView.peer.clearSelection()
    }
    def popupShowing = popupMenu.isShowing
    def focusPopup() {
      KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu)
      popupListView.requestFocusInWindow()
      popupListView.selectIndices(0)
    }
    override def fireTableStructureChanged() {
      super.fireTableStructureChanged()
      allModels.foreach(_.fireTableStructureChanged())
    }
    override def fireTableDataChanged() {
      super.fireTableDataChanged()
      allModels.foreach(_.fireTableDataChanged())
    }
    def paintTable(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) {
      {
        // Paint the row header table.
        val cMinL = math.min(rowHeaderColCount0, cMin)
        val cMaxL = math.min(rowHeaderColCount0 - 1, cMax)

        PivotTableUI.rowHeaderPaintGrid(g, table, rMin, rMax, cMinL, cMaxL, rowHeaderTableModel.getColumnCount - 1)
        PivotTableUI.rowHeaderPaintCells(g, table, rendererPane, rMin, rMax, cMinL, cMaxL)
      }
      {
        // Paint the main table.
        val cMinL = math.max(rowHeaderColCount0, cMin)
        val cMaxL = math.max(rowHeaderColCount0, cMax)

        PivotTableUI.mainPaintGrid(g,table, rMin, rMax, cMinL, cMaxL)
        PivotTableUI.rowHeaderPaintCells(g,table,rendererPane, rMin, rMax, cMinL, cMaxL)
      }
    }
    override def selectPopupValueIfOnlyOneShowing(r:Int, c:Int) {
      val (m,col) = getTableModel(c)
      m.selectPopupValueIfOnlyOneShowing(r, col)
    }
    override def singlePopupValue(r:Int, c:Int) = {
      val (m,col) = getTableModel(c)
      m.singlePopupValue(r, col)
    }

    private def getTableModel(columnIndex:Int) = {
      if (columnIndex < rowHeaderTableModel.getColumnCount) {
        (rowHeaderTableModel, columnIndex)
      } else {
        (mainTableModel, columnIndex - rowHeaderTableModel.getColumnCount)
      }
    }
  }

  def resizeRowHeaderColumns(topTable:JTable, bottomTable:JTable, rowHeaderTable:JTable, rowComponent:RowComponent,
                             rowFieldHeadingCount:Array[Int], sizerPanel:Panel, rowHeaderScrollPane:JScrollPane,
                             columnDetails:ColumnDetails) {
    val maxWidth = if (columnDetails.expandToFit) {
      Integer.MAX_VALUE
    } else {
      PivotJTable.MaxColumnWidth
    }
    val tmpLabel = new JLabel("")
    def getRowHeaderMaxColumnWidth(col:Int) = {
      val numRows = rowHeaderTable.getRowCount
      var max = -1
      var anyCollapsible = false
      for (row <- 0 until numRows) {
        val axisCell = rowHeaderTable.getValueAt(row, col).asInstanceOf[AxisCell]
        val width = {
          tmpLabel.setFont(PivotCellRenderer.selectFont(axisCell.value.value.value))
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
      max + (if (anyCollapsible) PivotCellRenderer.LeftIconWidth + 6 else 0)
    }

    val topTableModel = topTable.getColumnModel
    val bottomTableModel = bottomTable.getColumnModel
    val rowHeaderColumnModel = rowHeaderTable.getColumnModel
    val numCols = rowHeaderColCount0
    val widthToCheck = if (rowComponent.numberOfFields > 0) {
      rowComponent.guiField(0).initialPreferredSize.width
    } else {
      rowComponent.preferredSize.width
    }
    var col = 0
    if ((rowComponent.numberOfFields == 0) ||
            ((numCols == 1) && (rowComponent.numberOfFields > 0) &&
                    (getRowHeaderMaxColumnWidth(0) < widthToCheck))) {
      topTableModel.getColumn(0).setPreferredWidth(rowComponent.preferredSize.width)
      bottomTableModel.getColumn(0).setPreferredWidth(rowComponent.preferredSize.width)
      rowHeaderColumnModel.getColumn(0).setPreferredWidth(rowComponent.preferredSize.width)
    } else {
      for ((count, fieldIndex) <- rowFieldHeadingCount.zipWithIndex) {
        val widths = new Array[Int](count)
        for (c <- 0 until count) {
          val max = getRowHeaderMaxColumnWidth(col)
          val preferredWidth = math.min(max + 5, maxWidth)
          val topTableColumn = topTableModel.getColumn(col)
          val bottomTableColumn = bottomTableModel.getColumn(col)
          val rowHeaderColumn = rowHeaderColumnModel.getColumn(col)
          val widthToUse = preferredWidth
          topTableColumn.setPreferredWidth(widthToUse)
          bottomTableColumn.setPreferredWidth(widthToUse)
          rowHeaderColumn.setPreferredWidth(widthToUse)
          widths(c) = preferredWidth
          col += 1
        }

        val totalWidth = widths.sum
        val rowFieldComponent = rowComponent.guiField(fieldIndex)
        val fieldCompPrefWidth = rowFieldComponent.initialPreferredSize.width
        if (totalWidth < fieldCompPrefWidth) {
          // Need to add some space to each column.
          val perColDiff = ((fieldCompPrefWidth - totalWidth) / count)
          val startCol = col - count
          var newTotalWidth = 0
          for (c <- startCol until (startCol + count)) {
            val topTableColumn = topTableModel.getColumn(c)
            val bottomTableColumn = bottomTableModel.getColumn(c)
            val rowHeaderColumn = rowHeaderColumnModel.getColumn(c)
            val newWidth = widths(c - startCol) + perColDiff
            topTableColumn.setPreferredWidth(newWidth)
            bottomTableColumn.setPreferredWidth(newWidth)
            rowHeaderColumn.setPreferredWidth(newWidth)
            newTotalWidth += newWidth
          }
          if (newTotalWidth < fieldCompPrefWidth) {
            val extraWidth = fieldCompPrefWidth - newTotalWidth
            topTableModel.getColumn(0).setPreferredWidth(topTableModel.getColumn(0).getPreferredWidth + extraWidth)
            bottomTableModel.getColumn(0).setPreferredWidth(bottomTableModel.getColumn(0).getPreferredWidth + extraWidth)
            rowHeaderColumnModel.getColumn(0).setPreferredWidth(rowHeaderColumnModel.getColumn(0).getPreferredWidth + extraWidth)
          }
          rowFieldComponent.setPreferredWidth(rowFieldComponent.initialPreferredSize.width)
        } else {
          rowFieldComponent.setPreferredWidth(totalWidth)
        }
      }

      val firstColWidth = topTableModel.getColumn(0).getPreferredWidth + 2
      topTableModel.getColumn(0).setPreferredWidth(firstColWidth)
      bottomTableModel.getColumn(0).setPreferredWidth(firstColWidth)
      rowHeaderColumnModel.getColumn(0).setPreferredWidth(firstColWidth)
    }

    val rowComponentPreferredSize = {
      if (rowComponent.numberOfFields > 0) {
        (0 until rowComponent.numberOfFields).map(c => rowComponent.guiField(c).preferredSize.width).sum
      } else {
        rowComponent.preferredSize.width
      }
    }

    val width = math.max(rowComponentPreferredSize, (0 until numCols).map(c => rowHeaderColumnModel.getColumn(c).getPreferredWidth).sum)
    val height = colHeaderRowCount0 * PivotJTable.RowHeight
    val d = new Dimension(width - 1, 10)
    sizerPanel.preferredSize = d
    sizerPanel.minimumSize = d
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
      val r = bottomTableModel.getRowCount - 1
      bottomTableModel.fireTableRowsDeleted(r,r)
    }
  }

  def resizeColumnHeaderAndMainTableColumns(topTable:JTable, bottomTable:JTable, mainTable:JTable, colHeaderTable:JTable,
                                            colHeaderScrollPane:JScrollPane, columnHeaderScrollPanePanel:Panel,
                                            topTableScrollPane:JScrollPane, topTableScrollPanePanel:Panel,
                                            mainTableScrollPane:JScrollPane, columnDetails:ColumnDetails) {
    val numRows = mainTable.getRowCount
    val colOffset = rowHeaderColCount0
    val numCols = mainColCount0
    val rowOffset = colHeaderRowCount0
    val maxWidth = if (columnDetails.expandToFit) {
      Integer.MAX_VALUE
    } else {
      PivotJTable.MaxColumnWidth
    }

    val tmpLabel = new JLabel("")

    val errorIconSize = 20
    val mainColumnWidths = for (col <- 0 until numCols) yield {
      var max = PivotJTable.MinColumnWidth
      for (row <- 0 until numRows) {
        val tableCell = mainTable.getValueAt(row, col).asInstanceOf[TableCell]
        tmpLabel.setFont(PivotCellRenderer.selectFont(tableCell.value))
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
      tmpLabel.setFont(PivotCellRenderer.selectFont(axisCell.value.value.value))
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
              PivotCellRenderer.PlusIcon
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
        math.min(w + 1, maxWidth)
      })
    }

    val topTableColumnModel = topTable.getColumnModel
    val bottomTableColumModel = bottomTable.getColumnModel
    val mainColumnModel = mainTable.getColumnModel
    val colHeaderColumnModel = colHeaderTable.getColumnModel

    (maxColumnWidths.zipWithIndex).foreach{case (width,colIndex) => {
      topTableColumnModel.getColumn(colIndex + colOffset).setPreferredWidth(width)
      bottomTableColumModel.getColumn(colIndex + colOffset).setPreferredWidth(width)
      mainColumnModel.getColumn(colIndex).setPreferredWidth(width)
      colHeaderColumnModel.getColumn(colIndex).setPreferredWidth(width)
    }}

    val columnHeaderViewport = colHeaderScrollPane.getViewport
    val topTableViewport = topTableScrollPane.getViewport
    val prefHeight = colHeaderTable.getPreferredSize.height
    val d1 = new Dimension(colHeaderTable.getPreferredSize.width, prefHeight)
    columnHeaderViewport.setPreferredSize(d1)
    topTableViewport.setPreferredSize(d1)
    val d2 = new Dimension(PivotJTable.MinColumnWidth, prefHeight)
    columnHeaderViewport.setMinimumSize(d2)
    topTableViewport.setMinimumSize(d2)
    val d3 = new Dimension(Integer.MAX_VALUE, prefHeight)
    columnHeaderScrollPanePanel.maximumSize = d3
    topTableScrollPanePanel.maximumSize = d3
    mainTableScrollPane.getViewport.setPreferredSize(mainTable.getPreferredSize)
  }
}

case class NumberOfInPageEditsUpdated(numberOfEdits:Int, valid:Boolean) extends Event