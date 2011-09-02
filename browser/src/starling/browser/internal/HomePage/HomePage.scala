package starling.browser.internal.HomePage

import scala.swing.Swing._
import java.awt.{Font, Color, Dimension, Cursor}
import java.awt.event.KeyEvent
import org.jdesktop.swingx.painter.{CompoundPainter, GlossPainter}
import scala.swing._
import swing.event._
import javax.swing.{JComponent, KeyStroke}
import starling.browser.service.Version
import starling.browser._
import common._
import internal.{HelpPage, RootBrowserBundle, BookmarksPanel, BrowserIcons}

class HomePage

case object StarlingHomePage extends Page {

  def bundle = RootBrowserBundle.bundleName
  def build(version: Version) = HomePagePageData(version)
  def createServerContext(sc: ServerContext) = sc.browserService.version
  type SC = Version

  def createComponent(context: PageContext, data: PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PageData]) = new StarlingHomePageComponent(context, browserSize, data)
  def text = "Starling"
  def icon = BrowserIcons.im("/icons/weather-clear.png")
}

case class HomePagePageData(version:Version) extends PageData

class StarlingHomePageComponent(context:PageContext, browserSize:Dimension, pageData:PageData) extends MigPanel("insets 0") with PageComponent {
  private val data = pageData match {case d:HomePagePageData => {d}}

  private val bookmarksPanel = new BookmarksPanel(context)
  bookmarksPanel.background = GuiUtils.TaskPageButtonBackgroundColour
  bookmarksPanel.border = LineBorder(GuiUtils.TaskPageButtonBorderColour)
  val componentsBkColour = new Color(228, 231, 246)
  bookmarksPanel.bookmarksListView.background = componentsBkColour
  bookmarksPanel.dayPicker.background = componentsBkColour
  bookmarksPanel.goToBookmarkButton.background = GuiUtils.TaskPageBackgroundColour

  override def getState = {
    Some(StarlingHomePageComponentState(bookmarksPanel.bookmarksListView.selectedOption))
  }

  override def setState(state:Option[ComponentState]) {
    state match {
      case Some(StarlingHomePageComponentState(selectedBookmark)) => {
        bookmarksPanel.bookmarksListView.selectedOption = selectedBookmark
      }
      case _ =>
    }
  }

  private val versionPanel = new MigPanel("") {
    border = LineBorder(GuiUtils.TaskPageButtonBorderColour)
    background = GuiUtils.TaskPageButtonBackgroundColour

    val ver = data.version
    if (ver.production) {
      add(new Label("<html><b>Production</b></html>"), "ax center, gapleft 100lp, gapright 100lp")
    } else {
      add(new Label("<html><b>" + data.version.name + " (" + data.version.hostname + ")</b></html>"), "ax center, wrap")
      add(new Label(data.version.database), "span, ax center")
    }
  }

  val homeButtons = context.bundles.flatMap { bundle => { bundle.homeButtons(context) }}

  val c = new MigPanel("insets 0", "[grow,fill]", "[p]0[grow,fill]") {
    val banner = new MigXPanel("insets 0", "[p][p][p]push[p]") {
      background = GuiUtils.BannerColour
      val gp = new GlossPainter
      val sp = StripedCornerPainter(new Color(0,0,200))
      backgroundPainter = new CompoundPainter(sp,gp)

      val logoImage = BrowserIcons.im("/icons/small_sunny_bird2.png")
      val logo = new FixedImagePanel(logoImage)

      val nameLabel = new Label {
        text = "Starling"
        font = new Font("Lucida Sans", Font.PLAIN, 30)
      }
      val welcomeLabel = new Label {
        text = "W E L C O M E !"
        font = new Font("Dialog", Font.BOLD, 60)
        foreground = new Color(255,221,138)
      }
      val userImage = BrowserIcons.im("/icons/32x32_user_dark.png")
      val userButton = new NumberedButton(context.localCache.currentUserName, userImage,
        modifiers => {
          val userPages = context.bundles.flatMap(_.userPage(context))
          userPages match {
            case Nil =>
            case page :: Nil => context.goTo(page, modifiers)
            case many => throw new Exception("There is more than one user page: " + many)
          }
        }, false)
      userButton.label.font = new Font("Serif", Font.PLAIN, 20)

      add(logo)
      add(nameLabel, "ay bottom, gapbottom 5lp")
      add(welcomeLabel, "ay center, gapleft 20lp")
      add(userButton, "ay center, gapright " + GuiUtils.StandardLeftIndent)
    }

    val actionsPanelHolder = new MigPanel("insets dialog") {
      val actionsPanel = new StripedPanel("insets 0", "[grow][p][p][p][p][grow]", "[grow][p][p][grow 150][p]") {

        val helpLabelHolder = new MigPanel {
          background = new Color(0,0,0,0)
          opaque = false

          val helpImage = BrowserIcons.im("/icons/32x32_Help.png")
          add(new FixedImagePanel(helpImage) {
            cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
            reactions += {
              case MouseClicked(_,_,_,_,_) => {context.goTo(HelpPage) }
            }
            listenTo(mouse.clicks)
          }, "push,grow")
        }

        add(helpLabelHolder, "split, spanx, ax right, ay top, wrap")

        homeButtons.zipWithIndex.foreach { case (button, index) => {
          val isFirst = (index == 0)
          val isLast = (index == homeButtons.size-1)
          val modifiers = (if (isFirst) ", skip 1" else "") + (if(isLast) ", wrap unrel" else "")
          val nb = new NumberedButton(button.name, button.icon, (modifiers) => {
            context.createAndGoTo( (serverContext) => button.pageFactory.create(serverContext), modifiers=modifiers) },
            tooltip0 = button.tooltip)
          add(nb, "sg" + modifiers)
        }}
        //add(tradeDataButton, "sg, skip 1")

        add(bookmarksPanel, "skip 1, spanx 4, growx, wrap")
        add(versionPanel, "newline, split, spanx, ax center, gapbottom 5lp")
      }
      add(actionsPanel, "push,grow")
    }

    add(banner, "wrap")
    add(actionsPanelHolder)
  }

  homeButtons.foreach { button => {
    button.key.foreach { key =>
      val actionName = button.name + "Action"
      peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(key, actionName)
      peer.getActionMap.put(actionName, Action(actionName) {context.createAndGoTo( (sc) => button.pageFactory.create(sc))}.peer)
    }
  }}

  override def defaultComponentForFocus = Some(bookmarksPanel.bookmarksListView.peer)

  private val runAct = bookmarksPanel.goToBookmarkAction1
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_F9, 0), runAct.getValue(javax.swing.Action.NAME))
  peer.getActionMap.put(runAct.getValue(javax.swing.Action.NAME), runAct)


  add(c, "push,grow")

//  context.setDefaultButton(Some(reportsPanel.runButton))
}

case class StarlingHomePageComponentState(bookmarkSelected:Option[BookmarkData]) extends ComponentState
