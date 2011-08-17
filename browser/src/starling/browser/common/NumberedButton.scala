package starling.browser.common

import java.awt.image.BufferedImage
import swing.Swing._
import swing.Label
import java.awt.{Color, Cursor}
import swing.event.{MouseExited, MouseEntered, MouseClicked}

class NumberedButton(text:String, image:BufferedImage, buttonClicked:(Boolean) => Unit, useBlueText:Boolean=true,
                          number:Option[String]=None) extends MigPanel {
  background = GuiUtils.TaskPageButtonBackgroundColour
  border = LineBorder(GuiUtils.TaskPageButtonBorderColour)
  cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
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

  override def enabled_=(b:Boolean) = {
    super.enabled = b
    imagePanel.enabled = b
    label.enabled = b
  }

  reactions += {
    case MouseClicked(_,_,k,_,_) if enabled => {
      buttonClicked(k == scala.swing.event.Key.Modifier.Control)
      background = GuiUtils.TaskPageButtonBackgroundColour
    }
    case MouseEntered(_,_,_) if enabled => {background = GuiUtils.TaskPageButtonOverBackgroundColour}
    case MouseExited(_,_,_) if enabled => {background = GuiUtils.TaskPageButtonBackgroundColour}
  }
  listenTo(mouse.clicks, mouse.moves)
}
