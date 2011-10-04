package starling.browser.common

import java.awt.image.BufferedImage
import swing.Label
import java.awt.{Color, Cursor}
import swing.event.{MouseExited, MouseEntered, MouseClicked}
import starling.browser.Modifiers

class NumberedButton(text:String, image:BufferedImage, buttonClicked:(Modifiers) => Unit, useBlueText:Boolean=true,
                          number:Option[String]=None, tooltip0:Option[String]=None,
                          backgroundColour:Color=GuiUtils.TaskPageButtonBackgroundColour,
                          backgroundOverColour:Color=GuiUtils.TaskPageButtonOverBackgroundColour) extends MigPanel with RoundedBackground {

  background = backgroundColour
  border = RoundedBorder(GuiUtils.TaskPageButtonBorderColour)
  cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
  tooltip0.map(tt => tooltip = tt)
  val imagePanel = new FixedImagePanel(image)
  val label = if (useBlueText) {
    new Label("<html><u>" + text + "</u></html>") {
      foreground = Color.BLUE
      name = text
    }
  } else {
    new Label(text)
  }

  add(imagePanel)
  number match {
    case None =>
    case Some(num) => if (useBlueText) {
      add(new Label("<html><u>" + num + "</u></html>") {
        foreground = Color.BLUE
      }, "split, gapright 2lp")
    } else {
      add(new Label(num), "split, gapright 2lp")
    }
  }
  add(label)

  override def enabled_=(b:Boolean) {
    super.enabled = b
    imagePanel.enabled = b
    label.enabled = b
  }

  reactions += {
    case MouseClicked(_,_,k,_,_) if enabled => {
      buttonClicked(Modifiers.modifiersEX(k))
      background = backgroundColour
    }
    case MouseEntered(_,_,_) if enabled => {background = backgroundOverColour}
    case MouseExited(_,_,_) if enabled => {background = backgroundColour}
  }
  listenTo(mouse.clicks, mouse.moves)
}
