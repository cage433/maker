package starling.pivot.view.swing

import javax.swing._
import swing.Label
import starling.pivot._
import model._
import starling.reports.pivot.OptionalPeriodLabel
import starling.gui.{StarlingIcons}
import starling.quantity.{Quantity, Percentage}
import java.awt.Font
import starling.daterange.{Period, DateRange}
import org.jdesktop.swingx.renderer.{ComponentProvider, CellContext, LabelProvider}

object PivotCellRenderer {
  val ErrorIcon = StarlingIcons.icon("/icons/12x12_error.png")
  val PlusIcon = StarlingIcons.icon("/icons/10x10_add.png")
  val MinusIcon = StarlingIcons.icon("/icons/10x10_minus.png")
  val BlankIcon = StarlingIcons.icon("/icons/10x10_blank.png")
  val ValidationIcon = StarlingIcons.icon("/icons/10x10_exclamation.png")
  val LeftIconWidth = PlusIcon.getIconWidth
  val RightIconWidth = ErrorIcon.getIconWidth
  val StandardFont = new Label().font
  // It's crazy that I'm setting the size here, but on windows xp, the default size of 11 just looks awful with a monospace font!
  val MonoSpacedFont = new Font("Monospaced", Font.PLAIN, 12)
  def selectFont(value:Any) = {
    if (value.isInstanceOf[Period] || value.isInstanceOf[DateRange] || value.isInstanceOf[NullableDay] ||
            value.isInstanceOf[PivotQuantity] || value.isInstanceOf[OptionalPeriodLabel] || value.isInstanceOf[Percentage] ||
            value.isInstanceOf[NullablePeriod] || value.isInstanceOf[Quantity]) {
      MonoSpacedFont
    } else {
      StandardFont
    }
  }
}

import PivotCellRenderer._

class PivotCellProvider(indentColumns:Array[Boolean], columnDetails:ColumnDetails, tableModel:PivotJTableModel) extends LabelProvider {
  private val maxWidth = if (columnDetails.expandToFit) {
    Integer.MAX_VALUE
  } else {
    PivotJTable.MaxColumnWidth
  }
  override def format(context:CellContext) {
    val row = context.getRow
    val column = context.getColumn
    context.getValue match {
      case tableCell:TableCell => {
        rendererComponent.setFont(selectFont(tableCell.value))
        rendererComponent.setText(tableCell.text)
        if (rendererComponent.getPreferredSize.width > maxWidth) {
          rendererComponent.setToolTipText(tableCell.text)
        } else {
          rendererComponent.setToolTipText(null)
        }

        tableCell.textPosition match {
          case LeftTextPosition => rendererComponent.setHorizontalAlignment(SwingConstants.LEFT)
          case CenterTextPosition => rendererComponent.setHorizontalAlignment(SwingConstants.CENTER)
          case RightTextPosition => rendererComponent.setHorizontalAlignment(SwingConstants.RIGHT)
        }
        if (tableCell.isError || tableCell.warning.isDefined) {
          if (tableCell.isError) {
            rendererComponent.setIcon(PivotCellRenderer.ErrorIcon)
          } else {
            rendererComponent.setIcon(PivotCellRenderer.ValidationIcon)
          }
          rendererComponent.setHorizontalTextPosition(SwingConstants.LEFT)
        } else {
          rendererComponent.setIcon(null)
          rendererComponent.setHorizontalTextPosition(SwingConstants.TRAILING)
        }
      }
      case axisCell:AxisCell => {
        rendererComponent.setFont(selectFont(axisCell.value.value.value))
        rendererComponent.setText(axisCell.text)
        rendererComponent.setHorizontalTextPosition(SwingConstants.TRAILING)
        if (tableModel.rowHeader(row,column)) {
          // Row header
          axisCell.textPosition match {
            case LeftTextPosition => rendererComponent.setHorizontalAlignment(SwingConstants.LEFT)
            case RightTextPosition => rendererComponent.setHorizontalAlignment(SwingConstants.RIGHT)
          }
          axisCell.collapsible match {
            case None => if (indentColumns(column)) {
              rendererComponent.setIcon(BlankIcon)
            } else {
              rendererComponent.setIcon(null)
            }
            case Some(true) => rendererComponent.setIcon(PivotCellRenderer.PlusIcon)
            case Some(false) => rendererComponent.setIcon(PivotCellRenderer.MinusIcon)

          }
        } else {
          // Column header
          axisCell.collapsible match {
          case None => {
            rendererComponent.setHorizontalAlignment(SwingConstants.CENTER)
            rendererComponent.setIcon(null)
          }
          case Some(true) => {
            rendererComponent.setHorizontalAlignment(SwingConstants.LEFT)
            rendererComponent.setIcon(PivotCellRenderer.PlusIcon)
          }
          case Some(false) => {
            rendererComponent.setHorizontalAlignment(SwingConstants.LEFT)
            rendererComponent.setIcon(PivotCellRenderer.MinusIcon)
          }
          }
        }

        if (rendererComponent.getPreferredSize.width > maxWidth) {
          rendererComponent.setToolTipText(axisCell.text)
        } else {
          rendererComponent.setToolTipText(null)
        }
      }
    }
  }
}