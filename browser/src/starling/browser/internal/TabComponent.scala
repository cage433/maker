package starling.browser.internal

import java.awt.image.BufferedImage
import scala.swing.{Label, TabbedPane}
import java.awt.Dimension
import starling.browser.Page
import starling.browser.common.{TwoFixedImagePanel, FixedImagePanel, MigPanel}
import javax.swing.JPanel
import net.miginfocom.swing.MigLayout
import org.jdesktop.swingx.JXBusyLabel

class TabComponent(windowMethods:WindowMethods, tabbedPane:TabbedPane, initialText:String, initialIcon:BufferedImage) extends MigPanel("insets 0") {
  opaque = false

  private val iconPanel = new FixedImagePanel(initialIcon)

  private val label = new Label {
    text = initialText
    maximumSize = new Dimension(300, preferredSize.height)
  }

  private def closeTab() {
    if (windowMethods.canCloseTab) {
      val selectionToRemove = getCurrentPageNumber
      tabbedPane.pages.remove(selectionToRemove)
      val newSelection = if (selectionToRemove == tabbedPane.pages.length - 1) selectionToRemove - 1 else selectionToRemove
      tabbedPane.selection.index = newSelection
    }
  }

  private def getCurrentPageNumber = {
    val numTabs = tabbedPane.pages.length
    var keepGoing = true
    var tabIndexToClose = -1
    for (tabNum <- 0 until numTabs; if keepGoing) {
      if (tabbedPane.peer.getTabComponentAt(tabNum) == peer) {
        keepGoing = false
        tabIndexToClose = tabNum
      }
    }
    tabIndexToClose
  }

  def setTextFromPage(page:Page) {
    label.text = page.shortText
    iconPanel.image = page.icon
  }

  def setBusy(busy:Boolean) {
    iconHolderPanel.removeAll()
    if (busy) {
      iconHolderPanel.add(busyLabel)
      busyLabel.setBusy(true)
    } else {
      iconHolderPanel.add(iconPanel.peer)
      busyLabel.setBusy(false)
    }
    iconHolderPanel.revalidate()
  }

  private val closeButton = new TwoFixedImagePanel(
    BrowserIcons.im("/icons/close.png"),
    BrowserIcons.im("/icons/stop.png"),
    closeTab())

  private val iconHolderPanel = new JPanel(new MigLayout("insets 0")) {
    setOpaque(false)
  }
  private val busyLabel = new JXBusyLabel(new Dimension(16, 16)) {
    setOpaque(false)
  }
  iconHolderPanel.add(iconPanel.peer)

  add(iconHolderPanel)
  add(label)
  add(closeButton)
}

object TabComponent {
  def createNewTabComponent(createNewTab: => Unit) = new TwoFixedImagePanel(
    BrowserIcons.im("/icons/16x16_bullet_add.png"),
    BrowserIcons.im("/icons/16x16_bullet_add_down1.png"),
    createNewTab) {
    tooltip = "Open a new tab"
  }
}