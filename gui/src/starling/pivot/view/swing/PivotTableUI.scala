package starling.pivot.view.swing

import javax.swing.plaf.basic.BasicTableUI
import javax.swing.table.TableColumn
import starling.pivot.{OtherValueTotal, SubTotal, Total, NotTotal}
import javax.swing.{CellRendererPane, JTable, UIManager, JComponent}
import starling.pivot.model.AxisCell
import java.awt.{Color, Rectangle, Point, Graphics}
import starling.pivot.EditableCellState._
import starling.browser.common.GuiUtils._

object PivotTableUI {
  val GridColour = UIManager.getColor("Table.gridColor")
  
  def rowHeaderPaintGrid(g:Graphics, table:JTable, rMin:Int, rMax:Int, cMin:Int, cMax:Int, numCols:Int) {
    val minCell = table.getCellRect(rMin, cMin, true)
    val maxCell = table.getCellRect(rMax, cMax, true)
    val damagedArea = minCell.union(maxCell)
    val numRowsMinus1 = table.getRowCount - 1

    val cm = table.getColumnModel
    var y = damagedArea.y
    for (row <- rMin to rMax) {
      var x = damagedArea.x
      val rowHeight = table.getRowHeight(row)
      val yForHorizontalLine = y + rowHeight - 1
      for (col <- cMin to cMax) {
        val colWidth =  cm.getColumn(col).getWidth
        val xForVerticalLine = x + colWidth - 1
        val maxRowSelected = table.getSelectionModel.getMaxSelectionIndex
        if (table.isCellSelected(row,col) && (row != maxRowSelected)) {
          g.setColor(GridColour)
          g.drawLine(xForVerticalLine,y,xForVerticalLine,yForHorizontalLine)
          g.drawLine(x,yForHorizontalLine,xForVerticalLine,yForHorizontalLine)
        } else {
          val axisCell = table.getValueAt(row,col).asInstanceOf[AxisCell]
          // Horizontal line.
          val horizontalColour = if (row < numRowsMinus1) {
            val cellBelow = table.getValueAt(row + 1,col).asInstanceOf[AxisCell]
            cellBelow.totalState match {
              case NotTotal => if (cellBelow.shown) {
                GridColour
              } else {
                cellBelow.state match {
                  case Deleted => DeletedColour
                  case Edited => EditedCellColour
                  case Tainted => TaintedCellColour
                  case Added => AddedCellColour
                  case AddedBlank => AddedCellColour
                  case Normal => {
                    if (cellBelow.editable) {
                      RowHeaderEditableCellColour
                    } else {
                      PanelBackgroundColour
                    }
                  }
                  case Error => ErrorCellColour
                }
              }
              case other => GridColour
            }
          } else {
            GridColour
          }

          // Vertical line.
          val verticalColour = if (col == numCols) {
            GridColour
          } else {
            val cellRight = table.getValueAt(row, col + 1).asInstanceOf[AxisCell]
            if (!cellRight.hidden || (cellRight.offset > 0)) {
              GridColour
            } else {
              axisCell.totalState match {
                case NotTotal => PanelBackgroundColour
                case SubTotal => SubtotalColour
                case Total => TotalColour
                case OtherValueTotal => OtherValueTotalColour
              }
            }
          }

          // Decide which order to paint the lines so that for merged cells, we don't have a gap.
          if (horizontalColour == GridColour) {
            g.setColor(verticalColour)
            g.drawLine(xForVerticalLine,y,xForVerticalLine,yForHorizontalLine)
            g.setColor(horizontalColour)
            g.drawLine(x,yForHorizontalLine,xForVerticalLine,yForHorizontalLine)
          } else {
            g.setColor(horizontalColour)
            g.drawLine(x,yForHorizontalLine,xForVerticalLine,yForHorizontalLine)
            g.setColor(verticalColour)
            g.drawLine(xForVerticalLine,y,xForVerticalLine,yForHorizontalLine)
          }
        }
        x += colWidth
      }
      y += rowHeight
    }
  }

  def rowHeaderPaintCells(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) {
    val cm = table.getColumnModel
    val columnMargin = cm.getColumnMargin
    var cellRect:Rectangle = null
    var aColumn:TableColumn = null
    var columnWidth = 0
    for (row <- rMin to rMax) {
      cellRect = table.getCellRect(row, cMin, false)
      for (column <- cMin to cMax) {
        aColumn = cm.getColumn(column)
        columnWidth = aColumn.getWidth
        cellRect.width = columnWidth - columnMargin
        paintCell(g, table, rendererPane, cellRect, row, column, 0)
        cellRect.x += columnWidth
      }
    }
    rendererPane.removeAll()
  }

  def colHeaderPaintGrid(g:Graphics, table:JTable, rMin:Int, rMax:Int, cMin:Int, cMax:Int) {
    g.setColor(GridColour)
    val minCell = table.getCellRect(rMin, cMin, true)
    val maxCell = table.getCellRect(rMax, cMax, true)
    val damagedArea = minCell.union(maxCell)

    val numCols = table.getColumnCount - 1
    val numRows = table.getRowCount - 1
    val cm = table.getColumnModel

    def drawLeftVerticalLine(x:Int, y1:Int, y2:Int, col:Int) {
      if (col != 0) {
        g.drawLine(x,y1,x,y2)
      }
      if (col == numCols) {
        val w = x + cm.getColumn(col).getWidth
        g.drawLine(w,y1,w,y2)
      }
    }

    def drawBottomHorizontalLine(x1:Int, y:Int, x2:Int, row:Int) {
      if (row != numRows) {
        g.drawLine(x1,y,x2,y)
      }
    }

    var y = damagedArea.y - 1
    for (row <- rMin to rMax) {
      var x = damagedArea.x - 1
      val rowHeight = table.getRowHeight(row)
      val yForHorizontalLine = y + rowHeight
      for (col <- cMin to cMax) {
        val colWidth =  cm.getColumn(col).getWidth
        val xForVerticalLine = x + colWidth
        val axisCell = table.getValueAt(row,col).asInstanceOf[AxisCell]

        if (axisCell eq AxisCell.Filler) {
          drawBottomHorizontalLine(x,yForHorizontalLine,xForVerticalLine,row)
          drawLeftVerticalLine(x,y,yForHorizontalLine,col)
        } else {
          axisCell.totalState match {
            case NotTotal | OtherValueTotal => {
              if (axisCell.hidden) {
                drawBottomHorizontalLine(x,yForHorizontalLine,xForVerticalLine,row)
                if (col == numCols) {
                  g.drawLine(xForVerticalLine,y,xForVerticalLine,yForHorizontalLine)
                }
              } else {
                drawBottomHorizontalLine(x,yForHorizontalLine,xForVerticalLine,row)
                drawLeftVerticalLine(x,y,yForHorizontalLine,col)
              }
            }
            case SubTotal | Total => {
              drawLeftVerticalLine(x,y,yForHorizontalLine,col)
              drawBottomHorizontalLine(x,yForHorizontalLine,xForVerticalLine,row)
            }
          }
        }
        x += colWidth
      }
      y += rowHeight
    }
  }

  def colHeaderPaintCells(g:Graphics, table:JTable, rendererPane:CellRendererPane, rMin:Int, rMax:Int, cMin:Int, cMax:Int) {
    val cm = table.getColumnModel

    var cellRect:Rectangle = null

    for (row <- rMin to rMax) {
      val cMinToUse = cMin - table.getValueAt(row,cMin).asInstanceOf[AxisCell].offset

      val rightCell = table.getValueAt(row,cMax).asInstanceOf[AxisCell]
      val firstRightCellCol = cMax - rightCell.offset
      val firstRightCell = table.getValueAt(row, firstRightCellCol).asInstanceOf[AxisCell]
      val cMaxToUse = firstRightCellCol + firstRightCell.span.get - 1

      cellRect = table.getCellRect(row, cMinToUse, false)

      val listOfLists = (cMinToUse to cMaxToUse).foldLeft(List[List[Int]]())((list, i) => {
        table.getValueAt(row, i) match {
          case ac:AxisCell if ac.span.isDefined => List(i) :: list
          case _ => list.headOption.map(head => (head :+ i) :: list.tail).getOrElse(List(List(i)))
        }
      }).reverse

      for (columns <- listOfLists) {
        cellRect.width = columns.map(col => cm.getColumn(col).getWidth).sum
        paintCell(g,table,rendererPane,cellRect,row,columns.head,1)
        cellRect.x += cellRect.width
      }
    }
    rendererPane.removeAll()
  }

  def mainPaintGrid(g:Graphics, table:JTable, rMin:Int, rMax:Int, cMin:Int, cMax:Int) {
    g.setColor(GridColour)
    val minCell = table.getCellRect(rMin, cMin, true)
    val maxCell = table.getCellRect(rMax, cMax, true)
    val damagedArea = minCell.union(maxCell)

    // Horizontal lines.
    val tableWidth = damagedArea.x + damagedArea.width
    var y = damagedArea.y
    for (row <- rMin to rMax) {
      y += table.getRowHeight(row)
      g.drawLine(damagedArea.x, y - 1, tableWidth - 1, y - 1)
    }

    // Vertical lines.
    val cm = table.getColumnModel
    val tableHeight = damagedArea.y + damagedArea.height
    var x = damagedArea.x
    for (column <- cMin to cMax) {
      val w = cm.getColumn(column).getWidth
      x += w
      g.drawLine(x - 1, damagedArea.y, x - 1, tableHeight - 1)
    }
  }

  private def paintCell(g:Graphics, table:JTable, rendererPane:CellRendererPane, cellRect:Rectangle, row:Int, column:Int, reducedWidth:Int) {
    if (table.isEditing && table.getEditingRow == row && table.getEditingColumn == column) {
      val component = table.getEditorComponent
      component.setBounds(cellRect)
      component.validate()
    } else {
      val renderer = table.getCellRenderer(row, column)
      val component = table.prepareRenderer(renderer, row, column)
      rendererPane.paintComponent(g, component, table, cellRect.x, cellRect.y, cellRect.width-reducedWidth, cellRect.height, true)
    }
  }
}

class PivotTableUI extends BasicTableUI {
  override def paint(g:Graphics, c:JComponent) {
    val clip = g.getClipBounds
    val numRows = table.getRowCount
    val numCols = table.getColumnCount
    val bounds = table.getBounds()
    bounds.x = 0
    bounds.y = 0
    if (numRows > 0 && numCols > 0 && bounds.intersects(clip)) {
      val upperLeft = clip.getLocation
      val lowerRight = new Point(clip.x + clip.width - 1, clip.y + clip.height)
      val rMin = {
        val t = table.rowAtPoint(upperLeft)
        if (t == -1) 0 else t
      }
      val rMax = {
        val t = table.rowAtPoint(lowerRight)
        if (t == -1) numRows - 1 else t
      }
      val cMin = {
        val t = table.columnAtPoint(upperLeft)
        if (t == -1) 0 else t
      }
      val cMax = {
        val t = table.columnAtPoint(lowerRight)
        if (t == -1) numCols - 1 else t
      }

      table.getModel.asInstanceOf[PivotJTableModel].paintTable(g, table, rendererPane, rMin, rMax, cMin, cMax)
    }
  }
}