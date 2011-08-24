package starling.browser.common

import starling.browser.internal.BrowserIcons
import swing.{Swing, Alignment, Button}

/**
 * The button used for actions which change the page
 * It has an arrow on the right
 */
object NewPageButton {
  val arrowImage = BrowserIcons.icon("/icons/New_Page-16x16.png")
}
class NewPageButton extends Button {
  icon = NewPageButton.arrowImage
  horizontalTextPosition = Alignment.Left

  {
    peer.getActionListeners.toList foreach peer.removeActionListener
    peer.addActionListener(Swing.ActionListener { e =>
      publish(ButtonClickedEx(NewPageButton.this, e))
    })
  }
}
class ExButton(title0:String="") extends Button(title0) {
  {
    peer.getActionListeners.toList foreach peer.removeActionListener
    peer.addActionListener(Swing.ActionListener { e =>
      publish(ButtonClickedEx(ExButton.this, e))
    })
  }
}