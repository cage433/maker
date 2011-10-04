package starling.browser.internal

import javax.swing.plaf.metal.MetalTabbedPaneUI
import java.awt._
import geom._

class StarlingTabbedPaneUI extends MetalTabbedPaneUI {
  private val arcSize = 12
  private val startX = 5
  override def paintTopTabBorder(tabIndex:Int, g:Graphics, x:Int, y:Int, w:Int, h:Int, btm:Int, rght:Int, isSelected:Boolean) = {
    // I don't want to do anything to the last tab (the new tab)
    if (tabIndex != (tabPane.getTabCount - 1)) {
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      // This bit fills the gap if there is more than one row of tabs.
      val currentRun = getRunForTab(tabPane.getTabCount, tabIndex)
      val firstIndex = tabRuns(currentRun)
      if (shouldFillGap(currentRun, tabIndex, x, y)) {
        g2.translate(x, y)
        g2.setColor(getColorForGap(currentRun, x, y + 1))
        val end1 = w - 8
        val a = if (tabIndex != firstIndex) {
          val t = new CubicCurve2D.Float
          t.setCurve(end1-w + 1, 0, end1-w+11, 0, 1, h, 8, h)
          t
        } else {
          new Line2D.Float(-5,0,-5,h)
        }
        val a1 = new CubicCurve2D.Float
        a1.setCurve(0,h,8,h,6,0,16,0)
        val path2d1 = new Path2D.Float(a)
        path2d1.append(a1, true)
        g2.fill(path2d1)
        g2.translate(-x, -y)
      }

      // This bit draws the actual tabs.
      g2.translate(x, y)

      val q = new CubicCurve2D.Float
      q.setCurve(0, h, 8, h, 6, 0, 16, 0)
      g2.setColor(darkShadow)
      val end = w - 8
      val path2d = new Path2D.Float(q)
      val q1 = new CubicCurve2D.Float
      q1.setCurve(end, 0, end+10, 0, w, h, w + 8, h)
      path2d.append(q1, true)
      g2.draw(path2d)
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
      g2.translate(-x, -y)
    }
  }

  override def paintTabBackground(g:Graphics, tabPlacement:Int, tabIndex:Int, x:Int, y:Int, w:Int, h:Int, isSelected:Boolean) = {
    // I don't want to do anything to the last tab (the new tab)
    if (tabIndex != (tabPane.getTabCount - 1)) {
      if (isSelected) {
        g.setColor(selectColor)
      } else {
        g.setColor(tabPane.getBackgroundAt(tabIndex))
      }
      val g2 = g.asInstanceOf[Graphics2D]
      g2.translate(x,y)
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      val q = new CubicCurve2D.Float
      q.setCurve(0, h, 8, h, 6, 0, 16, 0)
      val end = w - 8
      val q1 = new CubicCurve2D.Float
      q1.setCurve(end, 0, end+10, 0, w, h, w + 8, h)
      val path2d = new Path2D.Float(q)
      path2d.append(q1, true)
      g2.fill(path2d)
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
      g2.translate(-x,-y)
    }
  }

  private val insetsRight = 0
  private val insetsLeft = 14
  private val _tabInsets = new Insets(2,insetsLeft,1,insetsRight)
  private val newTabInsets = new Insets(2,5,1,5)

  override def getTabInsets(tabPlacement:Int, tabIndex:Int) = {
    if (tabIndex == (tabPane.getTabCount - 1)) {
      newTabInsets
    } else {
      _tabInsets
    }
  }

  override def paintContentBorderTopEdge(g:Graphics, tabPlacement:Int, selectedIndex:Int, x:Int, y:Int, w:Int, h:Int) = {
    val oldColour = g.getColor
    g.setColor(darkShadow)
    if (selectedIndex >= 0) {
      val selectedRect = getTabBounds(selectedIndex, calcRect)
      val yOfSelectedTab = selectedRect.y + selectedRect.height

      // If the selected tab is not on the bottom row of tabs, just draw a full line.
      if (yOfSelectedTab != y) {
        g.drawLine(x,y,w,y)
      } else {
        val bottomLeftX = selectedRect.x
        val bottomRightX = bottomLeftX + selectedRect.width
        g.drawLine(x,y,bottomLeftX + 2,y)
        g.drawLine(bottomRightX + 6,y,w,y)
      }
    } else {
      // Not really sure if a tabbed pane with no selected tab is valid in this instance. Just draw the full line.
      g.drawLine(x,y,w,y)
    }
    g.setColor(oldColour)
  }
}
