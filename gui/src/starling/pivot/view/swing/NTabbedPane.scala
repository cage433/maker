package starling.pivot.view.swing

import starling.gui.pages.ConfigPanels
import swing.{Component, Label}
import swing.Swing._
import starling.gui.GuiUtils._
import starling.gui.GuiUtils
import javax.swing.border.AbstractBorder
import java.awt._
import java.awt.{Component => JComp}
import javax.swing.JPopupMenu
import swing.event.{MouseExited, MouseEntered, MousePressed, MouseClicked}

class NTabbedPane(configPanels:ConfigPanels, initiallyExpanded:Boolean)
        extends MigPanel("insets 1 0 0 1", "[fill,grow]", "[p]0[fill,grow]") {
  var expanded = initiallyExpanded
  var currentIndex = 0

  case class TabBorder(colour:Color = GuiUtils.BorderColour, borderInsetSize:Int = 1) extends AbstractBorder {
    var mouseOver = false
    override def paintBorder(c:JComp, g:Graphics, x:Int, y:Int, width:Int, height:Int) = {
      val g2 = g.asInstanceOf[Graphics2D]

      if (expanded) {
        val clip = g2.getClipBounds
        g2.setClip(clip.x, clip.y, clip.width, 10)
        val s = c.getSize
        val w = s.width - 1
        val h = s.height - 1
        val arc = 6
        val oldColour = g2.getColor
        g2.setColor(colour)
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        g2.drawRoundRect(0,0,w,h,arc,arc)
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
        g2.setClip(clip)
        g2.drawLine(x, y + 10, x, y + height-1)
        g2.drawLine(x+width-1, y + 10, x+width-1, y + height-1)        
        g2.setColor(oldColour)
      } else if (mouseOver) {        
        val s = c.getSize
        val w = s.width - 1
        val h = s.height - 1
        val arc = 6
        val oldColour = g2.getColor
        g2.setColor(colour)
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        g2.drawRoundRect(0,0,w,h,arc,arc)
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
        g2.setColor(oldColour)
      }
    }

    override def getBorderInsets(c:JComp, insets:Insets) = {
      insets.top = borderInsetSize
      insets.left = borderInsetSize
      insets.bottom = borderInsetSize
      insets.right = borderInsetSize
      insets
    }
    override def getBorderInsets(c:JComp) = new Insets(borderInsetSize,borderInsetSize,borderInsetSize,borderInsetSize)
  }

  case class TabPanelBorder(colour:Color = GuiUtils.BorderColour) extends AbstractBorder {
    override def paintBorder(c:JComp, g:Graphics, x:Int, y:Int, width:Int, height:Int) = {
      val g2 = g.asInstanceOf[Graphics2D]

      val origClip = g2.getClipBounds

      val selectedTab = tabs(currentIndex)
      val tabBounds = selectedTab.bounds

      val oldColour = g2.getColor
      g2.setColor(colour)

      val yPos = y + height - 1
      val xDelta = 4

      val x1 = tabBounds.x - xDelta
      val x2 = tabBounds.x + tabBounds.width-1+xDelta

      if (expanded) {
        // The bottom line missing out the area of the selected tab
        g2.drawLine(x, yPos, x1,yPos)
        g2.drawLine(x2, yPos, x+width-1,yPos)

        // The 1 pixel bits of the unselected tabs
        val bitY = yPos-1
        val unselectedTabs = tabs.filterNot(_ eq selectedTab)
        unselectedTabs.foreach(t => {
          val tBounds = t.bounds
          g2.drawLine(tBounds.x, bitY, tBounds.x, bitY)
          g2.drawLine(tBounds.x+tBounds.width-1, bitY, tBounds.x+tBounds.width-1, bitY)
        })

        // The curves for the selected tab
        val arc = 6
        val length = 10
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        g2.setClip(x1+1, yPos-xDelta, xDelta*2, xDelta*2)
        g2.drawRoundRect(x1-length+xDelta, yPos-length, length, length, arc, arc)
        g2.setClip(origClip)
        g2.setClip(x2-xDelta-1, yPos-xDelta, xDelta+1, xDelta+1)
        g2.drawRoundRect(x2-xDelta, yPos-length, length, length, arc, arc)
        g2.setClip(origClip)

        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
      } else {
        g2.drawLine(x, yPos, x+width-1,yPos)
      }
      g2.setColor(oldColour)
    }

    override def getBorderInsets(c:JComp, insets:Insets) = {
      insets.top = 0
      insets.left = 0
      insets.bottom = 1
      insets.right = 0
      insets
    }
    override def getBorderInsets(c:JComp) = new Insets(0,0,1,0)
  }

  class Tab(title:String, index:Int) extends MigPanel("insets 2lp n 2lp n") {
    val tb = TabBorder()
    border = tb
    reactions += {
      case MousePressed(_,_,_,1,_) => {
        if (expanded) {
          if (index != currentIndex) {
            changeTab(index)
          } else {
            hideTabs
          }
        } else {
          expanded = true
          changeTab(index)
          /*val popupMenu = new JPopupMenu
          popupMenu.add(configPanels.configPanels(index).peer)
          popupMenu.show(tabPanel.peer, 0, tabPanel.size.height-1)*/

        }
      }
      /*case MousePressed(_,_,_,2,_) => {
        if (expanded) {
          hideTabs
        } else {
          expandTabs(index)
        }
      }*/
      case MouseEntered(_,_,_) => tb.mouseOver = true; repaint()
      case MouseExited(_,_,_) => tb.mouseOver = false; repaint()
    }
    listenTo(mouse.clicks, mouse.moves)
    add(new Label(title), "pushy, growy, ay center")
  }

  val tabs = configPanels.configPanels.zipWithIndex.map{case (cp,index) => new Tab(cp.displayName, index)}

  val tabPanel = new MigPanel("insets 0 0 1 0") {
    border = TabPanelBorder(BorderColour)

    val blankSpace = new Label("  ")
    add(blankSpace)
    tabs.foreach(t => add(t, "gaptop 1lp, growy"))
    add(configPanels.extraComponent, "pushx, ax right")
  }

  val contentPanel = new MigPanel("insets 0") {
    def update(c:Component) {
      border = MatteBorder(0,0,1,0,BorderColour)
      removeAll
      add(c, "push,grow")
      revalidate
      repaint
    }

    def clear {
      border = EmptyBorder
      removeAll
      revalidate
      repaint
    }
  }

  def expandTabs(index:Int) {
    expanded = true
    changeTab(index)
  }

  def hideTabs {
    expanded = false
    contentPanel.clear
  }

  def changeTab(index:Int) {
    currentIndex = index
    if (expanded) {
      contentPanel.update(configPanels.configPanels(currentIndex))
    } else {
      hideTabs
    }
  }

  add(tabPanel, "wrap")
  add(contentPanel)

  changeTab(currentIndex)

  def state = NTabbedPaneState(currentIndex, expanded)
  def state_=(s:NTabbedPaneState) {
    expanded = s.expanded
    changeTab(s.index)
  }
}

case class NTabbedPaneState(index:Int, expanded:Boolean)