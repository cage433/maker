package starling.browser.internal.utilspage

import starling.browser._
import common.{NumberedButton, StripedPanel, GuiUtils, MigPanel}
import internal.{BrowserIcons, RootBrowserBundle}
import java.awt.{Color, Dimension}
import osgi.{BundleRemoved, BundleAdded}
import swing.Action
import javax.swing.JComponent

case object UtilsPage extends Page {
  def text = "Utils"
  def icon = BrowserIcons.im("/icons/16x16_utilities.png")
  def bundle = RootBrowserBundle.bundleName
  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PreviousPageData]) = new UtilsPageComponent(context)
  def build(serverContext:String) = null
  type SC = String
  def createServerContext(sc:ServerContext) = null
}

class UtilsPageComponent(context:PageContext) extends MigPanel("insets " + GuiUtils.StartPageInsets) with PageComponent {
  background = Color.WHITE
  val buttonPanel = new MigPanel("insets 0") {
    opaque = false

    def update(buttons:List[PageButton]) {
      removeAll
      buttons.zipWithIndex.foreach{case (button, i) => {
        val nb = new NumberedButton(button.name, button.icon, (modifiers) => {
          context.createAndGoTo( (serverContext) => button.pageFactory.create(serverContext), modifiers=modifiers) },
          tooltip0 = button.tooltip)
        val extraConstraints = if (((i % 3) == 0) && (i != 0)) {
          ",newline, split 3, spanx"
        } else {
          ""
        }
        add(nb, "ax center, sg" + extraConstraints)
      }}
      revalidate()
      repaint()
    }
  }
  val stripedPanel = new StripedPanel("insets 0", "[grow][p][grow]", "[grow][p][grow 150]") {
    add(buttonPanel, "newline, skip1")
  }

  private def addButtonActions(buttons:List[PageButton]) {
    buttons.foreach { button => {
      button.key.foreach { key =>
        val actionName = button.name + "Action"
        peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(key, actionName)
        peer.getActionMap.put(actionName, Action(actionName) {context.createAndGoTo( (sc) => button.pageFactory.create(sc))}.peer)
      }
    }}
  }

  private def updateButtons() {
    val utilButtons = context.bundles.flatMap { bundle => { bundle.utilButtons(context) }}
    buttonPanel.update(utilButtons)
    addButtonActions(utilButtons)
  }

  reactions += {
    case BundleAdded(_) => updateButtons()
    case BundleRemoved(_) => updateButtons()
  }
  listenTo(context.remotePublisher)

  updateButtons()

  add(stripedPanel, "push, grow")
}