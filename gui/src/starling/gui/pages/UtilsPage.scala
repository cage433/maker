package starling.gui.pages

import starling.gui._
import java.awt.{Dimension}
import starling.pivot.view.swing.{StripedPanel, MigPanel}
import javax.swing.{JComponent, KeyStroke}
import java.awt.event.KeyEvent
import swing.Action

case class UtilsPage() extends Page {
  def text = "Utils"
  def icon = StarlingIcons.im("/icons/16x16_utilities.png")
  def build(reader:PageBuildingContext) = null
  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension) = UtilsPageComponent(context)
}

case class UtilsPageComponent(context:PageContext) extends MigPanel("insets dialog") with PageComponent {
  val c = new StripedPanel("insets 0", "[grow][p][grow]", "[grow][p][grow 150]") {
    val statsImage = StarlingIcons.im("/icons/32x32_stats.png")
    val userStatsString = "1."
    def viewStats(ctrlDown:Boolean) {
      context.goTo(UserStatsPage(PivotPageState()), ctrlDown)
    }
    val statsButton = new ReferenceDataButton("View User Stats", statsImage, ctrlDown => viewStats(ctrlDown), number = Some(userStatsString))
    UtilsPageComponent.this.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
            put(KeyStroke.getKeyStroke(KeyEvent.VK_1, 0), userStatsString)
    UtilsPageComponent.this.peer.getActionMap.put(userStatsString, Action(userStatsString){viewStats(false)}.peer)

    val runAsUserImage = StarlingIcons.im("/icons/32x32_user_dark.png")
    val runAsUserString = "2."
    def runAsUser(ctrlDown:Boolean) {
      context.goTo(RunAsUserPage(), ctrlDown)
    }
    val runAsUserButton = new ReferenceDataButton("Run As User", runAsUserImage, ctrlDown => runAsUser(ctrlDown), number = Some(runAsUserString))
    UtilsPageComponent.this.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
            put(KeyStroke.getKeyStroke(KeyEvent.VK_2, 0), runAsUserString)
    UtilsPageComponent.this.peer.getActionMap.put(runAsUserString, Action(runAsUserString){runAsUser(false)}.peer)

    def gotoCannedPage(ctrlDown:Boolean) {
      context.goTo(CannedHomePage(), ctrlDown)
    }
    val cannedPageString = "3."
    val cannedPageImage = StarlingIcons.im("/icons/32x32_canned_launcher.png")
    val cannedPageButton = new ReferenceDataButton("Canned Page", cannedPageImage, (ctrlDown) => gotoCannedPage(ctrlDown), number = Some(cannedPageString))
    UtilsPageComponent.this.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
            put(KeyStroke.getKeyStroke(KeyEvent.VK_3, 0), cannedPageString)
    UtilsPageComponent.this.peer.getActionMap.put(cannedPageString, Action(cannedPageString){gotoCannedPage(false)}.peer)

    add(statsButton, "split, newline, skip1, sg")
    add(runAsUserButton, "sg")
    add(cannedPageButton, "sg")
  }
  add(c, "push, grow")
}