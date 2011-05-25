package starling.pivot.view.swing

import starling.pivot._
import starling.gui.GuiUtils._
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
          case AxisCell(_,_,_,_,_,SubTotal,_,_,_,_) => true
          case _ => false
        }
      }
    })

    val otherValueTotalAllTablesHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.totalState == OtherValueTotal => true
          case AxisCell(_,_,_,_,_,OtherValueTotal,_,_,_,_) => true
          case _ => false
        }
      }
    })

    val mainTableTotalHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.totalState == Total => true
          case AxisCell(_,_,_,_,_,Total,_,_,_,_) => true
          case _ => false
        }
      }
    })

    val mainTableSubtotalTotalHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.totalState == SubtotalTotal => true
          case AxisCell(_,_,_,_,_,SubtotalTotal,_,_,_,_) => true
          case _ => false
        }
      }
    })

    val negativeHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case v:TableCell => (v.doubleValueIgnoringErrors match {
            case None => false
            case Some(value) => if (v.state != EditableCellState.Error) (value < 0.0) else false
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

    val warningHighlighter = new IconHighlighter(new HighlightPredicate() {
      def isHighlighted(renderer: Component, adapter: ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.warning.isDefined=> true
          case _ => false
        }
      }
    }, StarlingIcons.ValidationError)

    val editedCellsHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.state == EditableCellState.Edited => true
          case AxisCell(_,_,_,_,_,_,_,_,_,EditableCellState.Edited) => true
          case _ => false
        }
      }
    })
    editedCellsHighlighter.setBackground(EditedCellColour)
    editedCellsHighlighter.setSelectedBackground(BlendedEditedCellColour)

    val editedErrorCellsHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.state == EditableCellState.Error => true
          case AxisCell(_,_,_,_,_,_,_,_,_,EditableCellState.Error) => true
          case _ => false
        }
      }
    })
    editedErrorCellsHighlighter.setBackground(ErrorCellColour)
    editedErrorCellsHighlighter.setSelectedBackground(BlendedErrorCellColour)

    val editableCellsHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.editable => true
          case AxisCell(_,_,_,_,_,_,_,_,true,_) => true
          case _ => false
        }
      }
    })
    editableCellsHighlighter.setBackground(EditableCellColour)
    editableCellsHighlighter.setSelectedBackground(BlendedEditableCellColour)

    val deletedCellsHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case t:TableCell if t.state == EditableCellState.Deleted => true
          case AxisCell(_,_,_,_,_,_,_,_,_,EditableCellState.Deleted) => true
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
          case t:TableCell if t.state == EditableCellState.Added => true
          case AxisCell(_,_,_,_,_,_,_,_,_,EditableCellState.Added) => true
          case _ => false
        }
      }
    })
    editedAddedCellsHighlighter.setBackground(AddedCellColour)
    editedAddedCellsHighlighter.setSelectedBackground(BlendedAddedCellColour)

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
    table.addHighlighter(editedErrorCellsHighlighter)
    table.addHighlighter(warningHighlighter)
  }
}