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

case class OverrideDetails(text:String, state:EditableCellState)
case class TableValue(value:AnyRef, row:Int, column:Int)

object PivotTableType extends Enumeration {
  type PivotTableType = Value
  val RowHeader, ColumnHeader, Main, Full = Value
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
  def textTyped(textField:JTextField, cellEditor:CellEditor , r:Int, c:Int, focusOwner:Option[AWTComp], tableFrom:AWTComp)
  def finishedEditing()
  def popupShowing:Boolean
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

    val totalRows = fullTableModel.getRowCount
    val totalColumns = fullTableModel.getColumnCount

    (0 until totalRows).find(r => {
      fullTableModel.getValueAt(r, 0) match {
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
            fullTableModel.getValueAt(r, c) match {
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

  def allEdits(e:PivotEdits):PivotEdits = fullTableModel.edits(e)

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
    fullTableModel.fireTableRowsUpdated(0, fullTableModel.getRowCount-1)

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
    type CellType = AxisCell
    def getRowCount = colHeaderData0.length
    def getColumnCount = rowHeaderData0(0).length
    def getValueAt(rowIndex:Int, columnIndex:Int) = AxisCell.Null
    def paintTable(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) {}
    def rowHeader(row:Int,col:Int) = false
    def collapseOrExpand(row:Int, col:Int, pivotTableView:PivotTableView) {}
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
      fullTableModel.fireTableRowsDeleted(start + colHeaderRowCount0, end + colHeaderRowCount0)
    }}
  }

  val mainTableModel = new PivotJTableModel {
    type CellType = TableCell

    private val addedRows0 = new ListBuffer[Array[TableCell]]
    private val blankCells = data0(0).map(_.copy(state = AddedBlank, text = "", longText = None))
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
        overrideMap -= k
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

      val (newRowCells, normalCells) = cells.partition{case (r,c) => getValueAt(r,c).state == Added}

      newRowCells.foreach{case (r,c) => {
        val value = getValueAt(r,c)
        val rowCell = rowHeaderTableModel.getValueAt(r,c)
        rowCell.value.childKey.value match {
          case NewRowValue(rowIndex) => {
            deleteEdits = deleteEdits.withNewAmended(rowIndex, field(c), None)
            overrideMap((r,c)) = value.copy(text = "", longText = Some(""), state = Deleted)
          }
          case _ => {
            val k = ((r,c))
            overrideMap(k) = overrideMap(k).copy(text = "", longText = Some(""))
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
        val value = tv.value
        val stringValue = value.asInstanceOf[String].trim
        val pars = parser(r, c)
        val currentValue = getValueAt(r,c)

        val (newValue,newLabel,newLabelForComparison,stateToUse) =  try {
          val uom = uoms0(c)
          val stringValueToUse = if ((uom != UOM.NULL) && stringValue.nonEmpty && stringValue.last.isDigit) {
            stringValue + " " + uom.asString
          } else {
            stringValue
          }

          val (v,t) = pars.parse(stringValueToUse, extraFormatInfo)

          val state = if (r < numOriginalRows && currentValue.state != Added) Edited else Added
          v match {
            case pq:PivotQuantity if t.isEmpty && (c < uoms0.length) => {
              val uom = uoms0(c)
              val dv = pq.doubleValue.get
              val newPQ = new PivotQuantity(dv, uom)
              (Some(newPQ), PivotFormatter.shortAndLongText(newPQ, extraFormatInfo, false)._1,PivotFormatter.shortAndLongText(newPQ, extraFormatInfo, true)._1,state)
            }
            case _ => (Some(v),t,t,state)
          }
        } catch {
          case e:Exception => (None, stringValue, stringValue, Error)
        }

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
          case Some(origVal) => fieldInfo.fieldToFormatter(field(c)).format(origVal, extraFormatInfo).text
        }

        if (originalLabel == newLabelForComparison) {
          anyResetEdits = resetCells(List(k), anyResetEdits, false)
        } else {
          if (stateToUse == Error) {
            overrideMap(k) = originalCell.copy(text = stringValue, longText = Some(stringValue), state = Error)
          } else {
            overrideMap(k) = originalCell.copy(text = newLabel, longText = Some(newLabel), state = stateToUse, value = newValue.get, textPosition = RightTextPosition)
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

  val popupMenu = new JPopupMenu {
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
      case KeyPressed(_,scala.swing.event.Key.Tab,_,_) => selectText(selection.items.head)
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
    override def parser(row:Int, col:Int) = {
      val (m,r,c) = getTableModel(row, col)
      m.parser(r,c)
    }

    private def getTableModels(cells:List[(Int,Int)]) = {
      cells.map{case (r,c) => {
        getTableModel(r,c)
      }}.groupBy(_._1)
    }

    override def setValuesAt(values:List[TableValue], currentEdits:PivotEdits, fireChange:Boolean) = {
      var anyResetEdits = currentEdits
      val modelToData = values.map(tv => {
        val r = tv.row
        val c = tv.column
        val (mod, row, col) = getTableModel(r, c)
        (mod, row, col, tv.value)
      }).groupBy(_._1)

      modelToData.foreach{case (mod, list) =>  {
        val valsForMod = list.map{case (_,r,c,v) => TableValue(v, r, c)}
        anyResetEdits = mod.setValuesAt(valsForMod, anyResetEdits, false)
      }}

      if (fireChange) {
        if (anyResetEdits != currentEdits) {
          val newEdits = edits(anyResetEdits)
          updateEdits(newEdits, Full)
        } else {
          tableUpdated()
        }
      }

      anyResetEdits
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
          updateEdits(newEdits, Full)
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


      println("")
      println("")
      println("ALL EDITS")
      println(updatedEdits)
      println("")
      println("")


      updatedEdits
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
          updateEdits(newEdits, Full)
        } else {
          tableUpdated()
        }
      }
      editsToUse
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
    override def mapCellToFieldsForMainTable(row:Int, col:Int) = {
      val offsetRow = row - colHeaderRowCount0
      val offsetCol = col - rowHeaderColCount0
      if ((offsetRow >= 0) && (offsetCol >= 0)) {
        mainTableModel.mapCellToFieldsForMainTable(offsetRow, offsetCol)
      } else {
        List()
      }
    }

    override def rowHeaderStrategySelection(row:Int, col:Int) = {
      val offsetRow = row - colHeaderRowCount0
      if (offsetRow >= 0) {
        rowHeaderTableModel.rowHeaderStrategySelection(offsetRow, col)
      } else {
        None
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

    val fullColumnModel = fullTable.getColumnModel
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
      fullColumnModel.getColumn(0).setPreferredWidth(rowComponent.preferredSize.width)
      rowHeaderColumnModel.getColumn(0).setPreferredWidth(rowComponent.preferredSize.width)
    } else {
      for ((count, fieldIndex) <- rowFieldHeadingCount.zipWithIndex) {
        val widths = new Array[Int](count)
        for (c <- 0 until count) {
          val max = getRowHeaderMaxColumnWidth(col)
          val preferredWidth = math.min(max + 5, maxWidth)
          val fullColumn = fullColumnModel.getColumn(col)
          val rowHeaderColumn = rowHeaderColumnModel.getColumn(col)
          val widthToUse = preferredWidth
          fullColumn.setPreferredWidth(widthToUse)
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
          rowFieldComponent.setPreferredWidth(rowFieldComponent.initialPreferredSize.width)
        } else {
          rowFieldComponent.setPreferredWidth(totalWidth)
        }
      }

      val firstColWidth = fullColumnModel.getColumn(0).getPreferredWidth + 2
      fullColumnModel.getColumn(0).setPreferredWidth(firstColWidth)
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
      val r = fullTableModel.getRowCount - 1
      fullTableModel.fireTableRowsDeleted(r,r)
    }
  }

  def resizeColumnHeaderAndMainTableColumns(fullTable:JTable, mainTable:JTable, colHeaderTable:JTable,
                                            colHeaderScrollPane:JScrollPane, columnHeaderScrollPanePanel:Panel,
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

case class NumberOfInPageEditsUpdated(numberOfEdits:Int, valid:Boolean) extends Event