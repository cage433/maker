package starling.pivot.view.swing

import collection.mutable.{HashMap,HashSet}
import java.awt.Component
import org.jdesktop.swingx.decorator.{ColorHighlighter, HighlightPredicate, ComponentAdapter, AbstractHighlighter}

class BoldFontHighlighter(predicate:HighlightPredicate) extends AbstractHighlighter(predicate) {
  def doHighlight(component:Component, adapter:ComponentAdapter) = {
    component.setFont(component.getFont.deriveFont(java.awt.Font.BOLD))
    component
  }
}

class UpdatingBackgroundColourHighlighter(predicate:HighlightPredicate, map:HashMap[(Int,Int),RefreshedCell]) extends AbstractHighlighter(predicate) {
  def doHighlight(component:Component, adapter:ComponentAdapter) = {
    val index = (adapter.column,adapter.row)
    if (map.keySet.contains(index)) {
      component.setBackground(map(index).currentColour)
    }
    component
  }
}

class BackgroundHighlighter(cells:HashSet[(Int,Int)]) extends ColorHighlighter(new HighlightPredicate{
  def isHighlighted(renderer:Component, adapter:ComponentAdapter) = cells((adapter.row, adapter.column))
})