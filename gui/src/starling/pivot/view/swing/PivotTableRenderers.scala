package starling.pivot.view.swing

import javax.swing._
import table.DefaultTableCellRenderer
import swing.Label
import starling.daterange.DateRange
import starling.pivot._
import model._
import starling.reports.pivot.OptionalPeriodLabel
import starling.gui.{StarlingIcons}
import starling.quantity.{Quantity, Percentage}
import java.awt.Font

class PivotTableRenderers

object MainTableCellRenderer {
  val ErrorIcon = StarlingIcons.icon("/icons/12x12_error.png")
  val PlusIcon = StarlingIcons.icon("/icons/10x10_add.png")
  val MinusIcon = StarlingIcons.icon("/icons/10x10_minus.png")
  val BlankIcon = StarlingIcons.icon("/icons/10x10_blank.png")
  val LeftIconWidth = PlusIcon.getIconWidth
  val RightIconWidth = ErrorIcon.getIconWidth
  val StandardFont = new Label().font
  // It's crazy that I'm setting the size here, but on windows xp, the default size of 11 just looks awful with a monospace font!
  val MonoSpacedFont = new Font("Monospaced", Font.PLAIN, 12)
  def selectFont(value:Any) = {
    if (value.isInstanceOf[DateRange] || value.isInstanceOf[NullableDay] ||
            value.isInstanceOf[PivotQuantity] || value.isInstanceOf[OptionalPeriodLabel] || value.isInstanceOf[Percentage] ||
            value.isInstanceOf[NullablePeriod] || value.isInstanceOf[Quantity]) {
      MonoSpacedFont
    } else {
      StandardFont
    }
  }
}

import MainTableCellRenderer._

class MainTableCellRenderer(indentColumns:Array[Boolean], maxWidth:Int) extends DefaultTableCellRenderer {
  override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int) = {

    super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
    value match {
      case tableCell:TableCell => {
        setFont(selectFont(tableCell.value))
        setText(tableCell.text)
        if (getPreferredSize.width > maxWidth) {
          setToolTipText(tableCell.text)
        } else {
          setToolTipText(null)
        }

        tableCell.textPosition match {
          case LeftTextPosition => setHorizontalAlignment(SwingConstants.LEFT)
          case CenterTextPosition => setHorizontalAlignment(SwingConstants.CENTER)
          case RightTextPosition => setHorizontalAlignment(SwingConstants.RIGHT)
        }
        if (tableCell.isError) {
          setIcon(MainTableCellRenderer.ErrorIcon)
          setHorizontalTextPosition(SwingConstants.LEFT)
        } else {
          setIcon(null)
          setHorizontalTextPosition(SwingConstants.TRAILING)
        }
      }
      case axisCell:AxisCell => {
        setFont(selectFont(axisCell.value.value.value))
        setText(axisCell.text)
        setHorizontalTextPosition(SwingConstants.TRAILING)
        if (table.getModel.asInstanceOf[PivotJTableModel].rowHeader(row,column)) {
          // Row header
          axisCell.textPosition match {
            case LeftTextPosition => setHorizontalAlignment(SwingConstants.LEFT)
            case RightTextPosition => setHorizontalAlignment(SwingConstants.RIGHT)
          }
          axisCell.collapsible match {
            case None => if (indentColumns(column)) {
              setIcon(BlankIcon)
            } else {
              setIcon(null)
            }
            case Some(true) => setIcon(MainTableCellRenderer.PlusIcon)
            case Some(false) => setIcon(MainTableCellRenderer.MinusIcon)

          }
        } else {
          // Column header
          axisCell.collapsible match {
          case None => {
            setHorizontalAlignment(SwingConstants.CENTER)
            setIcon(null)
          }
          case Some(true) => {
            setHorizontalAlignment(SwingConstants.LEFT)
            setIcon(MainTableCellRenderer.PlusIcon)
          }
          case Some(false) => {
            setHorizontalAlignment(SwingConstants.LEFT)
            setIcon(MainTableCellRenderer.MinusIcon)
          }
          }
        }

        if (getPreferredSize.width > maxWidth) {
          setToolTipText(axisCell.text)
        } else {
          setToolTipText(null)
        }
      }
    }
    this
  }
}