package starling.browser.common

import swing.{Alignment, Button}
import starling.browser.internal.BrowserIcons

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
}