package starling.gui

import javax.swing.ImageIcon
import swing.{Alignment, Button}

/**
 * The button used for actions which change the page
 * It has an arrow on the right
 */
object NewPageButton {
  val arrowImage = StarlingIcons.icon("/icons/New_Page-16x16.png")
}
class NewPageButton extends Button {
  icon = NewPageButton.arrowImage
  horizontalTextPosition = Alignment.Left
}