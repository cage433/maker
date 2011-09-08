package starling.pivot.view.swing

import collection.mutable.{HashMap,HashSet}
import java.awt.Component
import org.jdesktop.swingx.decorator.{ColorHighlighter, HighlightPredicate, ComponentAdapter, AbstractHighlighter}
import starling.browser.common.RichColour._

class BoldFontHighlighter(predicate:HighlightPredicate) extends AbstractHighlighter(predicate) {
  def doHighlight(component:Component, adapter:ComponentAdapter) = {
    component.setFont(component.getFont.deriveFont(java.awt.Font.BOLD))
    component
  }
}

class UpdatingBackgroundColourHighlighter(predicate:HighlightPredicate, map:HashMap[(Int,Int),RefreshedCell]) extends AbstractHighlighter(predicate) {
  def doHighlight(component:Component, adapter:ComponentAdapter) = {
    val index = (adapter.row,adapter.column)
    if (map.keySet.contains(index)) {
      val refreshedCell = map(index)
      val bg = component.getBackground.blend(refreshedCell.currentColour, 1.0f - (refreshedCell.currentColour.getAlpha / 255.0f))
      component.setBackground(bg)
    }
    component
  }
}

class BackgroundHighlighter(cells:HashSet[(Int,Int)]) extends ColorHighlighter(new HighlightPredicate{
  def isHighlighted(renderer:Component, adapter:ComponentAdapter) = cells((adapter.row, adapter.column))
})