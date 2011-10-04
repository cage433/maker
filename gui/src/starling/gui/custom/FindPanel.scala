package starling.gui.custom

import swing.Swing._
import java.awt.{Cursor, Graphics2D, Color}
import swing.event._
import swing.{Label, TextField}
import javax.swing.ImageIcon
import starling.gui.StarlingIcons
import starling.browser.common.{GuiUtils, FixedImagePanel, MigPanel}

class FindPanel extends MigPanel("insets 0", "[p][p]0[p]") {
  private val findIcon = new FixedImagePanel(StarlingIcons.im("/icons/16x16find.png"))

  private val textField = new TextField(35) {
    override protected def paintBorder(g:Graphics2D) = {
      super.paintBorder(g)
      val width = size.width - 1
      val height = size.height - 2
      g.setColor(Color.WHITE)
      g.drawLine(width, 1, width, height)
    }
  }

  private val clearImage = new FixedImagePanel(StarlingIcons.im("/icons/closeHovered.png")) {
    cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
  }
  private val clearImageHolder = new MigPanel("insets 0") {
    border = MatteBorder(1, 0, 1, 1, GuiUtils.BorderColour)
    background = Color.WHITE
    add(clearImage, "gapafter 2lp, align center center, push, grow")
  }

  private var oldText = ""

  reactions += {
    case MouseClicked(`clearImage`,_,_,_,_) => {
      resetText
    }    
    case KeyReleased(`textField`, _, _, _) => {
      val newText = findText
      if (oldText != newText) {
        oldText = newText
        publish(FindTextChanged(newText))
      }
    }
  }

  listenTo(clearImage.mouse.clicks, textField.keys)

  add(findIcon, "al center center, push, grow")
  add(textField, "grow")
  add(clearImageHolder, "grow")

  def resetText {
    textField.text = ""
    publish(FindTextChanged(textField.text))
  }

  def findFieldRequestFocus {
    textField.requestFocusInWindow
  }

  def findText = textField.text.trim
}

case class FindTextChanged(findText:String) extends Event
case class FilterTextChanged(filterText:String) extends Event

class FilterPanel extends MigPanel("insets 0", "[p][p]0[p]") {
  private val labelWithIcon = new Label {
    text = "Filter"
    icon = StarlingIcons.icon("/icons/16x16_filter.png")
  }

  private val textField = new TextField(35) {
    override protected def paintBorder(g:Graphics2D) = {
      super.paintBorder(g)
      val width = size.width - 1
      val height = size.height - 2
      g.setColor(Color.WHITE)
      g.drawLine(width, 1, width, height)
    }
  }
  private val clearImage = new FixedImagePanel(StarlingIcons.im("/icons/closeHovered.png")) {
    cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
  }
  private val clearImageHolder = new MigPanel("insets 0") {
    border = MatteBorder(1, 0, 1, 1, GuiUtils.BorderColour)
    background = Color.WHITE
    add(clearImage, "gapafter 2lp, align center center, push, grow")
  }

  private var oldText = ""

  reactions += {
    case MouseClicked(`clearImage`,_,_,_,_) => {
      resetText
    }
    case KeyReleased(`textField`, _, _, _) => {
      val newText = filterText
      if (oldText != newText) {
        oldText = newText
        publish(FilterTextChanged(newText))
      }
    }
  }

  listenTo(clearImage.mouse.clicks, textField.keys)

  add(labelWithIcon, "al center center, push, grow")
  add(textField, "grow")
  add(clearImageHolder, "grow")

  def resetText {
    textField.text = ""
    publish(FilterTextChanged(textField.text))
  }

  def filterFieldRequestFocus {
    textField.requestFocusInWindow
  }

  def filterText = textField.text.trim
}