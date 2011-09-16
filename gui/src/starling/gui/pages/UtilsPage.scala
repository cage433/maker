package starling.gui.pages

import starling.gui._
import javax.swing.{JComponent, KeyStroke}
import java.awt.event.KeyEvent
import starling.browser.internal.RunAsUserPage
import starling.browser._
import common.{GuiUtils, NumberedButton, StripedPanel, MigPanel}
import swing._
import java.awt.{Color, Dimension}

case class UtilsPage() extends StarlingServerPage {
  def text = "Utils"
  def icon = StarlingIcons.im("/icons/16x16_utilities.png")
  def build(reader:StarlingServerContext) = null
  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PreviousPageData]) = new UtilsPageComponent(context)
}

class UtilsPageComponent(context:PageContext) extends MigPanel("insets " + GuiUtils.StartPageInsets) with PageComponent {
  background = Color.WHITE
  val c = new StripedPanel("insets 0", "[grow][p][grow]", "[grow][p][grow 150]") {
    val statsImage = StarlingIcons.im("/icons/32x32_stats.png")
    val userStatsString = "1."
    def viewStats(modifiers:Modifiers) {
      context.goTo(UserStatsPage(PivotPageState()), modifiers)
    }
    val statsButton = new NumberedButton("View User Stats", statsImage, modifiers => viewStats(modifiers), number = Some(userStatsString))
    UtilsPageComponent.this.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
            put(KeyStroke.getKeyStroke(KeyEvent.VK_1, 0), userStatsString)
    UtilsPageComponent.this.peer.getActionMap.put(userStatsString, Action(userStatsString){viewStats(Modifiers.None)}.peer)

    val runAsUserImage = StarlingIcons.im("/icons/32x32_user_dark.png")
    val runAsUserString = "2."
    def runAsUser(modifiers:Modifiers) {
      context.goTo(RunAsUserPage(), modifiers)
    }
    val runAsUserButton = new NumberedButton("Run As User", runAsUserImage, ctrlDown => runAsUser(ctrlDown), number = Some(runAsUserString))
    UtilsPageComponent.this.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
            put(KeyStroke.getKeyStroke(KeyEvent.VK_2, 0), runAsUserString)
    UtilsPageComponent.this.peer.getActionMap.put(runAsUserString, Action(runAsUserString){runAsUser(Modifiers.None)}.peer)

    def gotoCannedPage(modifiers:Modifiers) {
      context.goTo(CannedHomePage(), modifiers)
    }
    val cannedPageString = "3."
    val cannedPageImage = StarlingIcons.im("/icons/32x32_canned_launcher.png")
    val cannedPageButton = new NumberedButton("Canned Page", cannedPageImage, (ctrlDown) => gotoCannedPage(ctrlDown), number = Some(cannedPageString))
    UtilsPageComponent.this.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
            put(KeyStroke.getKeyStroke(KeyEvent.VK_3, 0), cannedPageString)
    UtilsPageComponent.this.peer.getActionMap.put(cannedPageString, Action(cannedPageString){gotoCannedPage(Modifiers.None)}.peer)

    def gotoEventViewerPage(modifiers:Modifiers) {
      context.goTo(EventViewerPage())
    }
    val eventViewerString = "4."
    val eventViewerImage = StarlingIcons.im("/icons/32x32_event.png")
    val eventViewerButton = new NumberedButton("Event Viewer", eventViewerImage, (mods) => gotoEventViewerPage(mods), number = Some(eventViewerString))
    UtilsPageComponent.this.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
            put(KeyStroke.getKeyStroke(KeyEvent.VK_4, 0), eventViewerString)
    UtilsPageComponent.this.peer.getActionMap.put(eventViewerString, Action(eventViewerString){gotoEventViewerPage(Modifiers.None)}.peer)

    def gotoGitLogPage(modifiers:Modifiers) {
      context.goTo(GitLogPage(PivotPageState()))
    }
    val gitLogString = "5."
    val gitLogImage = StarlingIcons.im("/icons/32x32_log.png")
    val gitLogButton = new NumberedButton("Git Log", gitLogImage, (mods) => gotoGitLogPage(mods), number = Some(gitLogString))
    UtilsPageComponent.this.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
            put(KeyStroke.getKeyStroke(KeyEvent.VK_5, 0), gitLogString)
    UtilsPageComponent.this.peer.getActionMap.put(gitLogString, Action(gitLogString){gotoGitLogPage(Modifiers.None)}.peer)

    def gotoRabbitEventViewerPage(modifiers:Modifiers) {
      val latestRabbitEvent = context.localCache.localCache(LocalCacheKeys.LatestRabbitEvent)
      context.goTo(RabbitEventViewerPage(PivotPageState(), RabbitEventViewerPageState(latestRabbitEvent)))
    }
    val rabbitEventViewerString = "6."
    val rabbitEventViewerImage = StarlingIcons.im("/icons/32x32_event.png")
    val rabbitEventViewerButton = new NumberedButton("Rabbit Event Viewer", rabbitEventViewerImage, (mods) => gotoRabbitEventViewerPage(mods), number = Some(rabbitEventViewerString))
    UtilsPageComponent.this.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
            put(KeyStroke.getKeyStroke(KeyEvent.VK_6, 0), rabbitEventViewerString)
    UtilsPageComponent.this.peer.getActionMap.put(rabbitEventViewerString, Action(rabbitEventViewerString){gotoRabbitEventViewerPage(Modifiers.None)}.peer)

    val buttonHolder = new MigPanel("insets 0") {
      opaque = false
      add(statsButton, "sg")
      add(runAsUserButton, "sg")
      add(cannedPageButton, "sg, wrap")
      add(eventViewerButton, "split 3, spanx, ax center, sg")
      add(gitLogButton, "ax center, sg")
      add(rabbitEventViewerButton, "ax center, sg")
    }

    add(buttonHolder, "newline, skip1")
  }
  add(c, "push, grow")
}