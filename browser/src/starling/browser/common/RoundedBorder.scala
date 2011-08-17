package starling.browser.common

import javax.swing.border.AbstractBorder
import java.awt._

case class RoundedBorder(colour:Color = GuiUtils.BorderColour, borderInsetSize:Int = 1) extends AbstractBorder {
  override def paintBorder(c:Component, g:Graphics, x:Int, y:Int, width:Int, height:Int) {
    val g2 = g.asInstanceOf[Graphics2D]

    val s = c.getSize()
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

  override def getBorderInsets(c:Component, insets:Insets) = {
    insets.top = borderInsetSize
    insets.left = borderInsetSize
    insets.bottom = borderInsetSize
    insets.right = borderInsetSize
    insets
  }
  override def getBorderInsets(c:Component) = new Insets(borderInsetSize,borderInsetSize,borderInsetSize,borderInsetSize)
}