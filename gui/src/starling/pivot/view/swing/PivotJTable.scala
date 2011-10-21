package starling.pivot.view.swing

import collection.mutable.ListBuffer
import org.jdesktop.swingx.JXTable
import javax.swing._
import table.TableCellEditor
import text.JTextComponent
import swing.Swing._
import org.jdesktop.jxlayer.JXLayer
import java.awt.Cursor
import org.jdesktop.swingx.JXTable.GenericEditor
import javax.swing.TransferHandler.TransferSupport
import java.awt.datatransfer.{Clipboard, DataFlavor, StringSelection}
import starling.utils.Log
import starling.browser.common.GuiUtils._
import java.util.{StringTokenizer, Hashtable}
import swing.{MenuItem, Action}
import starling.pivot.model.{AxisCell, PivotTableModel}
import org.jdesktop.swingx.renderer.DefaultTableRenderer
import starling.browser.Modifiers
import java.awt.event._
import starling.pivot._

object PivotJTable {
  val RowHeight = 16
  val MinColumnWidth = 25
  val MaxColumnWidth = 200
}

class PivotJTable(tableModel:PivotJTableModel, pivotTableView:PivotTableView, model:PivotTableModel,
                  indentColumns:Array[Boolean], columnDetails:ColumnDetails) extends JXTable(tableModel) {
  setUI(new PivotTableUI)
  setAutoResizeMode(JTable.AUTO_RESIZE_OFF)
  getTableHeader.setReorderingAllowed(false)
  setFillsViewportHeight(true)
  setTableHeader(null)
  setCellSelectionEnabled(true)  
  setDefaultRenderer(classOf[Object], new DefaultTableRenderer(new PivotCellProvider(indentColumns, columnDetails, tableModel)))
  setRowHeight(PivotJTable.RowHeight)

  // If the delete key is pressed when more than one cell is selected, delete all deletable cells.
  addKeyListener(new KeyAdapter {
    override def keyPressed(e:KeyEvent) {
      if (e.getKeyCode == KeyEvent.VK_DELETE) {
        val selectedCells = getSelectedCells
        putClientProperty("JTable.autoStartsEdit", false)
        val deletableCells = selectedCells.filter{case (r,c) => {
          getValueAt(r,c) match {
            case ac:AxisCell => ac.editable && ac.state != EditableCellState.Deleted
            case tc:TableCell => tc.editable && tc.state != EditableCellState.Deleted
          }
        }}
        tableModel.deleteCells(deletableCells, true)
      } else if (e.getKeyCode == KeyEvent.VK_ESCAPE) {
        if (getCellEditor == null) {
          putClientProperty("JTable.autoStartsEdit", false)
          clearSelection()
        } 
      } else if (e.getKeyCode == KeyEvent.VK_S && (e.getModifiersEx & InputEvent.CTRL_DOWN_MASK) == InputEvent.CTRL_DOWN_MASK) {
        putClientProperty("JTable.autoStartsEdit", false)
        pivotTableView.publish(SavePivotEdits)
      } else if (e.getKeyCode == KeyEvent.VK_Z && (e.getModifiersEx & InputEvent.CTRL_DOWN_MASK) == InputEvent.CTRL_DOWN_MASK) {
        if (getCellEditor == null) {
          putClientProperty("JTable.autoStartsEdit", false)
        } else {
          e.consume()
          getCellEditor.cancelCellEditing()
        }
      } else if (e.getKeyCode == KeyEvent.VK_DOWN) {
        if (tableModel.popupShowing) {
          e.consume()
          tableModel.focusPopup()
        }
      } else if (e.getKeyCode == KeyEvent.VK_LEFT && (e.getModifiersEx & InputEvent.ALT_DOWN_MASK) == InputEvent.ALT_DOWN_MASK) {
        putClientProperty("JTable.autoStartsEdit", false)
      } else if (e.getKeyCode == KeyEvent.VK_RIGHT && (e.getModifiersEx & InputEvent.ALT_DOWN_MASK) == InputEvent.ALT_DOWN_MASK) {
        putClientProperty("JTable.autoStartsEdit", false)
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

    // Because of this bug: http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6759788 I have to have two canImport methods (canImport
    // and canReallyImport). It doesn't work perfectly but I can't do anything else until oracle fix the bug, which they never will.
    override def canImport(support:TransferSupport) = {
      val (startRow, startColumn) = if (support.isDrop) {
        // From a drag and drop.
        val dropLocation = support.getDropLocation.asInstanceOf[JTable.DropLocation]
        (dropLocation.getRow, dropLocation.getColumn)
      } else {
        // From a paste.
        val minRow = getSelectionModel.getMinSelectionIndex
        val minCol = getColumnModel.getSelectionModel.getMinSelectionIndex
        (minRow, minCol)
      }
      if (startRow >= 0 && startColumn >= 0 && startRow < getRowCount && startColumn < getColumnCount()) {
        getValueAt(startRow, startColumn) match {
          case tc:TableCell => tc.editable
          case ac:AxisCell => ac.editable
          case _ => false
        }
      } else {
        false
      }
    }

    def canReallyImport(support:TransferSupport) = {
      val (startRow, startColumn) = if (support.isDrop) {
        // From a drag and drop.
        val dropLocation = support.getDropLocation.asInstanceOf[JTable.DropLocation]
        (dropLocation.getRow, dropLocation.getColumn)
      } else {
        // From a paste.
        val minRow = getSelectionModel.getMinSelectionIndex
        val minCol = getColumnModel.getSelectionModel.getMinSelectionIndex
        (minRow, minCol)
      }
      if (startRow >= 0 && startColumn >= 0) {
        val textToInsert = support.getTransferable.getTransferData(DataFlavor.stringFlavor).asInstanceOf[String]
        val rows = textToInsert.split("\n").toList
        val cells = rows.map(rowText => {
          val tokenizer = if (rowText.contains("\t")) {
            new StringTokenizer(rowText, "\t")
          } else {
            new StringTokenizer(rowText)
          }
          val colBuffer = new ListBuffer[String]()
          while (tokenizer.hasMoreTokens) {
            val text = tokenizer.nextToken.trim().toLowerCase
            colBuffer += text
          }
          colBuffer.toList
        })

        val cellsInTable = cells.take(getRowCount - startRow).map(col => col.take(getColumnCount() - startColumn))

        val valuesCurrentlyInTable = cellsInTable.zipWithIndex.map{case (row, rowIndex) => {
          val realRow = startRow + rowIndex
          row.zipWithIndex.map{case (cell, colIndex) => {
            val realColumn = startColumn + colIndex
            getValueAt(realRow, realColumn)
          }}
        }}

        val allCellsEditable = valuesCurrentlyInTable.forall(row => {
          row.forall(cell => {
            cell match {
              case ac:AxisCell => ac.editable
              case tc:TableCell => tc.editable
            }
          })
        })

        if (allCellsEditable) {
          // Check that all text can be parsed.
          cellsInTable.zipWithIndex.map{case (row, rowIndex) => {
            val realRow = startRow + rowIndex
            row.zipWithIndex.map{case (cell, colIndex) => {
              val realColumn = startColumn + colIndex
              val parser = tableModel.parser(realRow, realColumn)
              try {
                parser.parse(cell, PivotFormatter.DefaultExtraFormatInfo)
                true
              } catch {
                case e => false
              }
            }}
          }}.forall(row => {
            row.forall(cell => cell)
          })
        } else {
          false
        }
      } else {
        false
      }
      true
    }
    override def exportToClipboard(comp:JComponent, clip:Clipboard, action:Int) {clip.setContents(new StringSelection(convertSelectedCellsToString), null)}
    override def importData(support:TransferSupport) = {
      val tableValuesToUpdate = new ListBuffer[TableValue]()
      if (canReallyImport(support)) {
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
                  tableValuesToUpdate += TableValue(text, realRow, realCol)
                }
                colCount += 1
              }
            }}
          }
        } catch {
          case t:Throwable => {
            Log.error("Unable to paste or drag data into table", t)
          }
        }
      }
      if (tableValuesToUpdate.nonEmpty) {
        tableModel.setValuesAt(tableValuesToUpdate.toList, None, true)
        true
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
        // Don't want to react if ctrl is down.
        val offMask = InputEvent.CTRL_DOWN_MASK
        if (ks.getKeyCode == KeyEvent.VK_UNDEFINED && ((e.getModifiersEx & offMask) == 0)) {
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
          } else if (e.getKeyCode == KeyEvent.VK_Z && ((e.getModifiersEx & InputEvent.CTRL_DOWN_MASK) == InputEvent.CTRL_DOWN_MASK)) {
            if (getCellEditor != null) {
              e.consume()
              getCellEditor.cancelCellEditing()
            }
          } else if (e.getKeyCode == KeyEvent.VK_LEFT && (e.getModifiersEx & InputEvent.ALT_DOWN_MASK) == InputEvent.ALT_DOWN_MASK) {
            if (getCellEditor != null) {
              e.consume()
            }
          } else if (e.getKeyCode == KeyEvent.VK_RIGHT && (e.getModifiersEx & InputEvent.ALT_DOWN_MASK) == InputEvent.ALT_DOWN_MASK) {
            if (getCellEditor != null) {
              e.consume()
            }
          }
        }
      })
    }

    val genericEditor = new GenericEditor(textField) {
      override def stopCellEditing() = {
        val r = getEditingRow
        val c = getEditingColumn

        val t = textField.getText.trim()

        val parser = tableModel.parser(r, c)
        val myRes = try {
          parser.parse(t, PivotFormatter.DefaultExtraFormatInfo)
          true
        } catch {
          case e => false
        }

        if (myRes) {
          super.stopCellEditing()
        } else {
          false
        }
      }

      override def getTableCellEditorComponent(table:JTable, value:AnyRef, isSelected:Boolean, row:Int, column:Int) = {
        val c = super.getTableCellEditorComponent(table, value, isSelected, row, column).asInstanceOf[JTextComponent]
        value match {
          case tc:TableCell => c.setText(tc.text)
          case ac:AxisCell => c.setText(ac.text)
        }
        c
      }
    }

    temp.put(classOf[Object], genericEditor)
//    temp.put(classOf[Number], new NumberEditorExt(true))
//    temp.put(classOf[Boolean], new BooleanEditor())
  }

  override def removeEditor() {
    super.removeEditor()
    tableModel.finishedEditing()
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

  def setSelectedCells(cells:List[(Int,Int)]) {
    for ((row,col) <- cells) {
      if (row >= 0 && row < getRowCount && col >= 0 && col < getColumnCount()) {
        addRowSelectionInterval(row,row)
        addColumnSelectionInterval(col,col)
      }
    }
  }

  addMouseListener(new MouseAdapter {
    override def mousePressed(e:MouseEvent) {
      val point = e.getPoint
      val table = e.getSource.asInstanceOf[JXTable]
      val (row, col) = (table.rowAtPoint(point), table.columnAtPoint(point))
      if ((row != -1) && (col != -1)) {
        if (SwingUtilities.isLeftMouseButton(e)) {
          if (e.getClickCount == 2) {
            val tableSelection = tableModel.mapCellToFieldsForMainTable(row, col)
            if (tableSelection.nonEmpty) {
              val modifiers = Modifiers.modifiersEX(e.getModifiersEx)
              pivotTableView.publish(TableDoubleClickEvent(model.getCurrentPivotFieldsState.filters, tableSelection, modifiers))
            } else {
              // We are doing a hack here that if we have double clicked on a strategy value in the row header table, change the strategy.
              tableModel.rowHeaderStrategySelection(row, col) match {
                case None =>
                case Some(stratInfo) => {

                  val currentFieldState = model.getCurrentPivotFieldsState
                  val filtersWithoutStrategy = currentFieldState.filters.filterNot(_._1 == stratInfo.field)
                  val newFilters = (stratInfo.field, SomeSelection(Set(stratInfo.selectedValue))) :: filtersWithoutStrategy
                  val strategyDepth:(Field, (Int, Int)) = currentFieldState.treeDepths.find(_._1 == stratInfo.field).get
                  val maxDepth = model.treeDetails.maxTreeDepths(stratInfo.field)
                  val min = strategyDepth._2._1
                  val max = strategyDepth._2._2
                  if (max != maxDepth) {
                    val newStrategyDepth = (stratInfo.field, (min + 1, max + 1))
                    val newTreeDepths = currentFieldState.treeDepths + newStrategyDepth

                    val newFieldState = currentFieldState.copy(filters = newFilters, treeDepths = newTreeDepths)
                    pivotTableView.publish(FieldsChangedEvent(newFieldState))
                  }

                }
              }
            }
          } else if (e.getClickCount == 1) {
            val cellRect = table.getCellRect(row, col, true)
            table.getValueAt(row, col) match {
              case tableCell:TableCell => {
                val error = tableCell.isError
                // If the click happened on the error icon, show the error page, else do a normal selection.
                if (error) {
                  val iconWidth = PivotCellRenderer.RightIconWidth
                  if (point.x > (cellRect.x + cellRect.width - (iconWidth + 2))) {
                    // Clicked on the error icon.
                    pivotTableView.publish(ShowErrorsEvent(tableCell.errors))
                  }
                }
              }
              case axisCell:AxisCell => {
                if (axisCell.collapsible.isDefined) {
                  val iconWidth = PivotCellRenderer.LeftIconWidth
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
              case ac:AxisCell => ac.editable && ac.state != EditableCellState.Deleted &&
                      (if (ac.state == EditableCellState.AddedBlank || ac.state == EditableCellState.Added) ac.label.nonEmpty && (row0 < getRowCount-1) else true)
              case tc:TableCell => tc.editable && tc.state != EditableCellState.Deleted &&
                      (if (tc.state == EditableCellState.AddedBlank || tc.state == EditableCellState.Added) tc.text.nonEmpty && (row0 < getRowCount-1) else true)
            }
          }}

          val resetableCells = selectedCells.filter{case (row0,col0) => {
            getValueAt(row0,col0) match {
              case ac:AxisCell => (ac.state != EditableCellState.Normal && ac.state != EditableCellState.Added && ac.state != EditableCellState.AddedBlank)
              case tc:TableCell => (tc.state != EditableCellState.Normal && tc.state != EditableCellState.Added && tc.state != EditableCellState.AddedBlank)
            }
          }}

          val popup = new JPopupMenu
          popup.setBorder(LineBorder(BorderColour))

          if (deletableCells.nonEmpty || resetableCells.nonEmpty) {
            if (deletableCells.nonEmpty) {
              val deleteActionName = if (deletableCells.size == 1) "Delete Cell" else "Delete Cells"
              val deleteAction = Action(deleteActionName) {
                tableModel.deleteCells(deletableCells, true)
              }
              val deleteItem = new MenuItem(deleteAction)
              popup.add(deleteItem.peer)
            }
            if (resetableCells.nonEmpty) {
              val resetActionName = if (resetableCells.size == 1) "Reset Cell" else "Reset Cells"
              val resetAction = Action(resetActionName) {
                tableModel.resetCells(resetableCells, true)
              }
              val resetItem = new MenuItem(resetAction)
              popup.add(resetItem.peer)
            }
          }

          popup.show(e.getComponent, point.x, point.y)
        }
      }
    }
  })
  addMouseMotionListener(new MouseMotionAdapter {
    override def mouseMoved(e:MouseEvent) {
      val point = e.getPoint
      val table = e.getSource.asInstanceOf[JXTable]
      val (row, col) = (table.rowAtPoint(point), table.columnAtPoint(point))
      if ((row != -1) && (col != -1)) {
        val cellRect = table.getCellRect(row, col, true)
        table.getValueAt(row, col) match {
          case tableCell:TableCell => {
            if (tableCell.isError) {
              val iconWidth = PivotCellRenderer.RightIconWidth
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
              val iconWidth = PivotCellRenderer.LeftIconWidth
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

  override def doFind() {}

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
      comp.selectAll()
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
        requestFocus()
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
        editorComp.requestFocus()
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
    sb.toString()
  }
}