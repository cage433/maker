package starling.gui

import swing.{MainFrame, Component}

/**
 * Bit of code to show a single widget
 * Just used when developing widgets
 */

class ShowWidget(widget:Component) extends MainFrame {
  title = "Show widget"

  contents = widget
  pack
  visible = true
}
