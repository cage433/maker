package starling.gui

import swing.{Button, ToggleButton}
import starling.browser.common.NewPageButton
import swing.Swing._
import java.awt.Graphics2D
import java.awt.image.BufferedImage

abstract class NewPageToolBarButton extends NewPageButton {
  border = CompoundBorder(border, EmptyBorder(0, 16, 0, 0))
  focusable = false
  val leftIcon:BufferedImage
  override protected def paintComponent(g:Graphics2D) {
    super.paintComponent(g)
    val startY = (size.height - leftIcon.getHeight) / 2
    g.drawImage(leftIcon, 4, startY, leftIcon.getWidth, leftIcon.getHeight, null)
  }
}

class ToolBarButton extends Button {
  focusable = false
}

class ToggleToolBarButton extends ToggleButton {
  focusable = false
}