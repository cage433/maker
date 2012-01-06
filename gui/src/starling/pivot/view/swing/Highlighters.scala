package starling.pivot.view.swing

import starling.pivot._
import starling.browser.common.GuiUtils._
import starling.pivot.model.AxisCell
import starling.quantity.{SpreadOrQuantity, Quantity}
import java.awt.{Component, Color}
import starling.gui.StarlingIcons
import org.jdesktop.swingx.decorator.{ComponentAdapter, IconHighlighter, HighlightPredicate, ColorHighlighter}
import javax.swing.{ImageIcon, Icon}

object Highlighters {
  def applyHighlighters(table:PivotJTable) {

    val subtotalAllTablesHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.totalState == SubTotal => true
          case a:AxisCell if a.totalState == SubTotal => true
          case _ => false
        }
      }
    })

    val otherValueTotalAllTablesHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.totalState == OtherValueTotal => true
          case a:AxisCell if a.totalState == OtherValueTotal => true
          case _ => false
        }
      }
    })

    val mainTableTotalHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.totalState == Total => true
          case a:AxisCell if a.totalState == Total => true
          case _ => false
        }
      }
    })

    val mainTableSubtotalTotalHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.totalState == SubtotalTotal => true
          case a:AxisCell if a.totalState == SubtotalTotal => true
          case _ => false
        }
      }
    })

    val negativeHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case v:TableCell => (v.doubleValueIgnoringErrors match {
            case None => false
            case Some(value) => if (!v.state.error) (value < 0.0) else false
          })
          case ac:AxisCell => ac.value.value.value match {
            case q:Quantity => {
              q.value < 0.0
            }
            case soq:SpreadOrQuantity => {
              soq.either match {
                case Left(q) => q.value < 0.0
                case Right(sq) => sq.front.value < 0.0 && sq.front.value < 0.0
              }
            }
            case _ => false
          }
        }
      }
    })

    val editedCellsHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.state.state == EditableCellState.Edited => true
          case a:AxisCell if a.state.state == EditableCellState.Edited => true
          case _ => false
        }
      }
    })
    editedCellsHighlighter.setBackground(EditedCellColour)
    editedCellsHighlighter.setSelectedBackground(BlendedEditedCellColour)

    val editedErrorCellsHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.state.invalid => true
          case a:AxisCell if a.state.invalid => true
          case _ => false
        }
      }
    })
    editedErrorCellsHighlighter.setBackground(ErrorCellColour)
    editedErrorCellsHighlighter.setSelectedBackground(BlendedErrorCellColour)

    val editedTaintedCellsHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.state.state == EditableCellState.Tainted => true
          case a:AxisCell if a.state.state == EditableCellState.Tainted => true
          case _ => false
        }
      }
    })
    editedTaintedCellsHighlighter.setBackground(TaintedCellColour)
    editedTaintedCellsHighlighter.setSelectedBackground(BlendedTaintedCellColour)

    val editableCellsHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.editable => true
          case a:AxisCell if a.editable => true
          case _ => false
        }
      }
    })
    editableCellsHighlighter.setBackground(EditableCellColour)
    editableCellsHighlighter.setSelectedBackground(BlendedEditableCellColour)

    val deletedCellsHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.state.state == EditableCellState.Deleted => true
          case a:AxisCell if a.state.state == EditableCellState.Deleted => true
          case _ => false
        }
      }
    })
    deletedCellsHighlighter.setBackground(DeletedColour)
    deletedCellsHighlighter.setForeground(Color.GRAY)
    deletedCellsHighlighter.setSelectedBackground(BlendedDeletedColour)
    deletedCellsHighlighter.setSelectedForeground(Color.GRAY)

    val editedAddedCellsHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.state.state == EditableCellState.Added => true
          case a:AxisCell if a.state.state == EditableCellState.Added => true
          case _ => false
        }
      }
    })
    editedAddedCellsHighlighter.setBackground(AddedCellColour)
    editedAddedCellsHighlighter.setSelectedBackground(BlendedAddedCellColour)

    val editedAddedBlankCellsHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.state.state == EditableCellState.AddedBlank => true
          case a:AxisCell if a.state.state == EditableCellState.AddedBlank => true
          case _ => false
        }
      }
    })
    editedAddedBlankCellsHighlighter.setBackground(AddedBlankCellColour)
    editedAddedBlankCellsHighlighter.setSelectedBackground(BlendedAddedBlankCellColour)

    val mainAxisCellBackgroundHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case ac:AxisCell => true
          case _ => false
        }
      }
    })
    mainAxisCellBackgroundHighlighter.setBackground(PanelBackgroundColour)
    mainAxisCellBackgroundHighlighter.setSelectedBackground(BlendedHeaderColour)

    val mainTableCellBackgroundHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case tc:TableCell => true
          case _ => false
        }
      }
    })
    mainTableCellBackgroundHighlighter.setBackground(Color.WHITE)

    negativeHighlighter.setForeground(Color.RED)
    negativeHighlighter.setSelectedForeground(Color.RED)
    subtotalAllTablesHighlighter.setBackground(SubtotalColour)
    subtotalAllTablesHighlighter.setSelectedBackground(BlendedSubtotalColour)
    otherValueTotalAllTablesHighlighter.setBackground(OtherValueTotalColour)
    otherValueTotalAllTablesHighlighter.setSelectedBackground(BlendedOtherValueTotalColour)
    mainTableTotalHighlighter.setBackground(TotalColour)
    mainTableTotalHighlighter.setSelectedBackground(BlendedTotalColour)
    mainTableSubtotalTotalHighlighter.setBackground(SubtotalTotalColour)
    mainTableSubtotalTotalHighlighter.setSelectedBackground(BlendedSubtotalTotalColour)

    table.addHighlighter(mainAxisCellBackgroundHighlighter)
    table.addHighlighter(mainTableCellBackgroundHighlighter)
    table.addHighlighter(otherValueTotalAllTablesHighlighter)
    table.addHighlighter(subtotalAllTablesHighlighter)
    table.addHighlighter(mainTableTotalHighlighter)
    table.addHighlighter(mainTableSubtotalTotalHighlighter)
    table.addHighlighter(editableCellsHighlighter)
    table.addHighlighter(negativeHighlighter)
    table.addHighlighter(editedCellsHighlighter)
    table.addHighlighter(deletedCellsHighlighter)
    table.addHighlighter(editedAddedCellsHighlighter)
    table.addHighlighter(editedAddedBlankCellsHighlighter)
    table.addHighlighter(editedErrorCellsHighlighter)
    table.addHighlighter(editedTaintedCellsHighlighter)
  }
}