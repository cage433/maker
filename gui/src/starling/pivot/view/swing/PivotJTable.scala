package starling.pivot.view.swing

import collection.mutable.ListBuffer
import org.jdesktop.swingx.JXTable
import javax.swing._
import table.{TableModel, TableCellEditor}
import text.JTextComponent
import swing.Swing._
import org.jdesktop.jxlayer.JXLayer
import java.awt.Cursor
import starling.pivot.{EditableCellState, TableCell}
import org.jdesktop.swingx.table.NumberEditorExt
import org.jdesktop.swingx.JXTable.{BooleanEditor, GenericEditor}
import javax.swing.TransferHandler.TransferSupport
import java.awt.datatransfer.{Clipboard, DataFlavor, StringSelection}
import starling.utils.Log
import starling.gui.GuiUtils
import java.util.{StringTokenizer, Hashtable}
import swing.{ScrollPane, ListView, MenuItem, Action}
import starling.pivot.model.{EditableInfo, AxisCell, PivotTableModel}
import java.awt.event._

object PivotJTable {
  val RowHeight = 16
  val MinColumnWidth = 50
  val MaxColumnWidth = 200
}

import PivotJTable._

class PivotJTable(tableModel:PivotJTableModel, pivotTableView:PivotTableView, model:PivotTableModel,
                  indentColumns:Array[Boolean]) extends JXTable(tableModel) {
  setUI(new PivotTableUI)
  setAutoResizeMode(JTable.AUTO_RESIZE_OFF)
  getTableHeader.setReorderingAllowed(false)
  setFillsViewportHeight(true)
  setTableHeader(null)
  setCellSelectionEnabled(true)  
  setDefaultRenderer(classOf[Object], new MainTableCellRenderer(indentColumns, MaxColumnWidth))
  setRowHeight(PivotJTable.RowHeight)

  // If the delete key is pressed when more than one cell is selected, delete all deletable cells.
  addKeyListener(new KeyAdapter {
    override def keyPressed(e:KeyEvent) {
      if (e.getKeyCode == KeyEvent.VK_DELETE) {
        val selectedCells = getSelectedCells
        putClientProperty("JTable.autoStartsEdit", false)
        val editableCells = selectedCells.filter{case (r,c) => {
          getValueAt(r,c) match {
            case ac:AxisCell => ac.editable
            case tc:TableCell => tc.editable
          }
        }}
        tableModel.deleteCells(editableCells)
      } else if (e.getKeyCode == KeyEvent.VK_S && (e.getModifiersEx & InputEvent.CTRL_DOWN_MASK) == InputEvent.CTRL_DOWN_MASK) {
        putClientProperty("JTable.autoStartsEdit", false)
        pivotTableView.publish(SavePivotEdits)
      } else if (e.getKeyCode == KeyEvent.VK_DOWN) {
        if (tableModel.popupShowing) {
          e.consume()
          tableModel.focusPopup()
        }
      } else {
        putClientProperty("JTable.autoStartsEdit", true)
      }
    }
  })

  setDragEnabled(true)

  setTransferHandler(new TransferHandler {
    override def createTransferable(c:JComponent) = {
      new StringSelection(convertSelectedCellsToString)
    }

    override def getSourceActions(c:JComponent) = {
      TransferHandler.COPY
    }

    override def canImport(support:TransferSupport) = {
      def checkValue(r:Int,c:Int) = {
        if (r >= 0 && c >= 0) {
          getValueAt(r,c) match {
            case ac:AxisCell => ac.editable
            case tc:TableCell => tc.editable
          }
        } else {
          false
        }
      }
      if (support.isDrop) {
        // From a drag and drop.
        val dropLocation = support.getDropLocation.asInstanceOf[JTable.DropLocation]
        checkValue(dropLocation.getRow, dropLocation.getColumn)
      } else {
        // From a paste.
        val minRow = getSelectionModel.getMinSelectionIndex
        val minCol = getColumnModel.getSelectionModel.getMinSelectionIndex
        checkValue(minRow, minCol)
      }
    }
    override def exportToClipboard(comp:JComponent, clip:Clipboard, action:Int) = clip.setContents(new StringSelection(convertSelectedCellsToString), null)
    override def importData(support:TransferSupport) = {
      if (canImport(support)) {
        try {
          val (startRow,startCol) = if (support.isDrop) {
            val dropLocation = support.getDropLocation.asInstanceOf[JTable.DropLocation]
            (dropLocation.getRow, dropLocation.getColumn)
          } else {
            (getSelectionModel.getMinSelectionIndex, getColumnModel.getSelectionModel.getMinSelectionIndex)
          }

          val textToInsert = support.getTransferable.getTransferData(DataFlavor.stringFlavor).asInstanceOf[String]
          val rows = textToInsert.split("\n").toList

          if (rows.nonEmpty) {
            rows.zipWithIndex.foreach{case (rowText,rowIndex) => {
              val realRow = startRow + rowIndex
              val tokenizer = if (rowText.contains("\t")) {
                new StringTokenizer(rowText, "\t")
              } else {
                new StringTokenizer(rowText)
              }
              var colCount = 0
              while (tokenizer.hasMoreTokens) {
                val text = tokenizer.nextToken
                val realCol = startCol + colCount
                if (realCol < getColumnCount()) {
                  setValueAt(text, realRow, realCol)
                }
                colCount += 1
              }
            }}
            true
          } else {
            false
          }
        } catch {
          case t:Throwable => {
            Log.error("Unable to paste or drag data into table", t)
            false
          }
        }
      } else {
        false
      }
    }
  })

  override def createDefaultEditors() {
    defaultEditorsByColumnClass = new UIDefaults(3, 0.75f)
    val temp = defaultEditorsByColumnClass.asInstanceOf[Hashtable[AnyRef,AnyRef]]

    val textField = new JTextField() {
      override def processKeyBinding(ks:KeyStroke, e:KeyEvent, condition:Int, pressed:Boolean) = {
        val r = super.processKeyBinding(ks, e, condition, pressed)
        if (ks.getKeyCode == KeyEvent.VK_UNDEFINED) {
          val focusOwner = if (isFocusOwner) {
            Some(this)
          } else if (PivotJTable.this.isFocusOwner) {
            Some(PivotJTable.this)
          } else {
            None
          }

          val r = getEditingRow
          val c = getEditingColumn

          tableModel.textTyped(this, PivotJTable.this.getCellEditor, r, c, focusOwner, PivotJTable.this)
        }
        r
      }

      addKeyListener(new KeyAdapter {
        override def keyPressed(e:KeyEvent) {
          if (e.getKeyCode == KeyEvent.VK_DOWN && tableModel.popupShowing) {
            e.consume()
            tableModel.focusPopup()
          }
        }
      })
    }

    temp.put(classOf[Object], new GenericEditor(textField))
    temp.put(classOf[Number], new NumberEditorExt(true))
    temp.put(classOf[Boolean], new BooleanEditor())
  }

  override def removeEditor() {
    super.removeEditor()
    tableModel.finishedEditing
  }

  def getSelectedCells = {
    val rowStart = getSelectedRow
    val rowEnd = getSelectionModel.getMaxSelectionIndex
    val colStart = getSelectedColumn
    val colEnd = getColumnModel.getSelectionModel.getMaxSelectionIndex
    val sel = new ListBuffer[(Int,Int)]
    if (rowStart == -1 || rowEnd == -1 || colStart == -1 || colEnd == -1) {
      sel.toList
    } else {
      for (row <- (rowStart to rowEnd); col <- (colStart to colEnd)) {
        if (isCellSelected(row, col)) {
          sel += ((row,col))
        }
      }
      sel.toList
    }
  }

  def setSelectedCells(cells:List[(Int,Int)]) = {
    for ((row,col) <- cells) {
      if (row >= 0 && row < getRowCount && col >= 0 && col < getColumnCount()) {
        addRowSelectionInterval(row,row)
        addColumnSelectionInterval(col,col)
      }
    }
  }

  addMouseListener(new MouseAdapter {
    override def mousePressed(e:MouseEvent) = {
      val point = e.getPoint
      val table = e.getSource.asInstanceOf[JXTable]
      val (row, col) = (table.rowAtPoint(point), table.columnAtPoint(point))
      if ((row != -1) && (col != -1)) {
        if (SwingUtilities.isLeftMouseButton(e)) {
          if (e.getClickCount() == 2) {
            val tableSelection = tableModel.mapCellToFields(row, col)
            if (tableSelection.nonEmpty) {
              val controlDown = (e.getModifiers & InputEvent.CTRL_MASK) == InputEvent.CTRL_MASK
              pivotTableView.publish(TableDoubleClickEvent(model.getCurrentPivotFieldsState.filters, tableSelection, controlDown))
            }
          } else if (e.getClickCount() == 1) {
            val cellRect = table.getCellRect(row, col, true)
            table.getValueAt(row, col) match {
              case tableCell:TableCell => {
                val error = tableCell.isError
                // If the click happened on the error icon, show the error page, else do a normal selection.
                if (error) {
                  val iconWidth = MainTableCellRenderer.RightIconWidth
                  if (point.x > (cellRect.x + cellRect.width - (iconWidth + 2))) {
                    // Clicked on the error icon.
                    pivotTableView.publish(ShowErrorsEvent(tableCell.errors))
                  }
                }
              }
              case axisCell:AxisCell => {
                if (axisCell.collapsible.isDefined) {
                  val iconWidth = MainTableCellRenderer.LeftIconWidth
                  if (point.x < (cellRect.x + iconWidth + 4)) {
                    tableModel.collapseOrExpand(row, col, pivotTableView)
                  }
                }
              }
            }
          }
        } else if (SwingUtilities.isRightMouseButton(e)) {
          // If the cell that was clicked on is outside the current selection, select just it, otherwise leave the selection as is.
          if (!getSelectedCells.contains((row,col))) {
            setRowSelectionInterval(row,row)
            setColumnSelectionInterval(col,col)
          }

          val selectedCells = getSelectedCells

          val deletableCells = selectedCells.filter{case (row0,col0) => {
            getValueAt(row0,col0) match {
              case ac:AxisCell => ac.editable && ac.state != EditableCellState.Deleted
              case tc:TableCell => tc.editable && tc.state != EditableCellState.Deleted
            }
          }}

          val resetableCells = selectedCells.filter{case (row0,col0) => {
            getValueAt(row0,col0) match {
              case ac:AxisCell => (ac.state != EditableCellState.Normal) && (if (ac.state == EditableCellState.Added) ac.label.nonEmpty else true)
              case tc:TableCell => (tc.state != EditableCellState.Normal) && (if (tc.state == EditableCellState.Added) tc.text.nonEmpty else true)
            }
          }}

          if (deletableCells.nonEmpty || resetableCells.nonEmpty) {
            val popup = new JPopupMenu
            popup.setBorder(LineBorder(GuiUtils.BorderColour))

            if (deletableCells.nonEmpty) {
              val deleteActionName = if (deletableCells.size == 1) "Delete Cell" else "Delete Cells"
              val deleteAction = Action(deleteActionName) {
                tableModel.deleteCells(deletableCells)
              }
              val deleteItem = new MenuItem(deleteAction)
              popup.add(deleteItem.peer)
            }

            if (resetableCells.nonEmpty) {
              val resetActionName = if (resetableCells.size == 1) "Reset Cell" else "Reset Cells"
              val resetAction = Action(resetActionName) {
                tableModel.resetCells(resetableCells)
              }
              val resetItem = new MenuItem(resetAction)
              popup.add(resetItem.peer)
            }

            popup.show(e.getComponent, point.x, point.y)
          }
        }
      }
    }
  })
  addMouseMotionListener(new MouseMotionAdapter {
    override def mouseMoved(e:MouseEvent) = {
      val point = e.getPoint
      val table = e.getSource.asInstanceOf[JXTable]
      val (row, col) = (table.rowAtPoint(point), table.columnAtPoint(point))
      if ((row != -1) && (col != -1)) {
        val cellRect = table.getCellRect(row, col, true)
        table.getValueAt(row, col) match {
          case tableCell:TableCell => {
            if (tableCell.isError) {
              val iconWidth = MainTableCellRenderer.RightIconWidth
              if (point.x > (cellRect.x + cellRect.width - (iconWidth + 2))) {
                setCursor0(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR))
              } else {
                setCursor0(Cursor.getDefaultCursor)
              }
            } else {
              setCursor0(Cursor.getDefaultCursor)
            }
          }
          case axisCell:AxisCell => {
            if (axisCell.shown && axisCell.collapsible.isDefined) {
              val iconWidth = MainTableCellRenderer.LeftIconWidth
              if (point.x < (cellRect.x + iconWidth + 4)) {
                setCursor0(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR))
              } else {
                setCursor0(Cursor.getDefaultCursor)
              }
            } else {
              setCursor0(Cursor.getDefaultCursor)
            }
          }
        }
      } else {
        setCursor0(Cursor.getDefaultCursor)
      }
      def setCursor0(cursor:Cursor) {
        getParent match {
          case jx:JXLayer[_] => {
            // We are wrapped in a JXLayer so we need to set the cursor this way.
            val gp = jx.getGlassPane
            if (gp.getCursor != cursor) {
              gp.setCursor(cursor)
            }
          }
          case _ => {
            // Just the standard table.
            if (getCursor != cursor) {
              setCursor(cursor)
            }
          }
        }
      }
    }
  })

  override def doFind = {}

  // Preparing the editor like this means that when you start typing the text already there is replaced. However, this messes up the F2 behaviour.
  override def prepareEditor(editor:TableCellEditor, row:Int, column:Int) = {
    val e = super.prepareEditor(editor, row, column)
    if (e.isInstanceOf[JTextComponent]) {
      val comp = e.asInstanceOf[JTextComponent]
      val text = {
        val t = comp.getText.trim
        if (t.startsWith("(") && t.endsWith(")")) {
          "-" + t.substring(1, t.length - 1)
        } else {
          t
        }
      }
      comp.setText(text)
      comp.selectAll
    }
    e
  }

  // This action is basically copied from BasicTableUI. It has been modified to ensure that we have excel like editing abilities on the table.
  private val editTableWithF2Action = Action("EditTableWithF2") {

    def getAdjustedLead(row:Boolean, model:ListSelectionModel) = {
      val index = model.getLeadSelectionIndex
      val compare = if (row) getRowCount else getColumnCount()
      if (index < compare) index else -1
    }

    val leadRow = getAdjustedLead(true, getSelectionModel)
    val leadColumn = getAdjustedLead(false, getColumnModel.getSelectionModel)

    if (!hasFocus) {
      val cellEditor = getCellEditor
      if (cellEditor != null && !cellEditor.stopCellEditing) {

      } else {
        requestFocus
      }
    } else {
      editCellAt(leadRow, leadColumn, null)
      // Because we've overridden prepareEditor to select all the text, we need to deselect the text here as we don't want that to happen when
      // F2 is pressed.
      val editorComp = getEditorComponent
      if (editorComp != null) {
        if (editorComp.isInstanceOf[JTextComponent]) {
          val tc = editorComp.asInstanceOf[JTextComponent]
          onEDT(onEDT{
            tc.setCaretPosition(tc.getText.length)
          })
        }
        editorComp.requestFocus
      }
    }
  }

  getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_F2,0), "EditTableWithF2")
  getActionMap.put("EditTableWithF2", editTableWithF2Action.peer)

  def convertSelectedCellsToString = {
    val ls = System.getProperty("line.separator")
    val sb = new StringBuilder
    val mainTableModel = getModel
    val mainColumns = getSelectedColumns
    val mainRows = getSelectedRows

    var firstRow = true
    for (row <- mainRows) {
      var firstColumn = true
      if (firstRow) firstRow = false else sb.append(ls)
      for (column <- mainColumns) {
        if (firstColumn) firstColumn = false else sb.append("\t")
        sb.append(mainTableModel.getValueAt(row, column) match {
          case cell:TableCell if cell.asString.nonEmpty => cell.asString
          case axisCell:AxisCell if axisCell.text.nonEmpty => axisCell.text
          case _ => " "
        })
      }
    }
    sb.toString
  }
}