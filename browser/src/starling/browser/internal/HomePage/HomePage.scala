package starling.browser.internal.HomePage

import java.awt.{Font, Color, Dimension, Cursor, RenderingHints, Toolkit}
import java.awt.event.KeyEvent
import scala.swing._
import swing.event._
import javax.swing.{JComponent, KeyStroke}
import starling.browser.service.Version
import starling.browser._
import common._
import internal.{HelpPage, RootBrowserBundle, BookmarksPanel, BrowserIcons}
import osgi.{BundleRemoved, BundleAdded}
import scala.util.Random
import java.awt.datatransfer.StringSelection

class HomePage

case object StarlingHomePage extends Page {

  def bundle = RootBrowserBundle.bundleName
  def build(x: String) = NoPageData
  def createServerContext(sc: ServerContext) = ""
  type SC = String

  def createComponent(context: PageContext, data: PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PreviousPageData]) = new StarlingHomePageComponent(context, browserSize)
  def text = "Starling"
  def icon = BrowserIcons.im("/icons/weather-clear.png")
}

object StarlingHomePageComponent {
  private val subTitles = Array("OLAP Platform", "Risk Management Framework", "Risk Management System", "Risk", "OLAP Reporting System", "Risk OLAP")
  private val r = new Random()
  def subTitle = subTitles(r.nextInt(subTitles.length))
}

class StarlingHomePageComponent(context:PageContext, browserSize:Dimension) extends MigPanel("insets 0") with PageComponent {
  private val bookmarksPanel = new BookmarksPanel(context) with RoundedBackground
  bookmarksPanel.background = GuiUtils.TaskPageButtonBackgroundColour
  bookmarksPanel.border = RoundedBorder(GuiUtils.TaskPageButtonBorderColour)
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

  private val versionPanel = new MigPanel("") with RoundedBackground {
    border = RoundedBorder(GuiUtils.TaskPageButtonBorderColour)
    background = new Color(0,0,0,32)

    val ver = context.localCache.version
    if (ver.production) {
      add(new Label("<html><b>Production</b></html>"), "ax center, gapleft 100lp, gapright 100lp")
    } else {
      val gitCommitLabel = new Label(ver.gitCommit) {
        tooltip = "Click on me to copy my contents to the clipboard"
        reactions += {
          case MouseClicked(_,_,_,_,_) => {
            val stringSelection = new StringSelection(ver.gitCommit)
            Toolkit.getDefaultToolkit.getSystemClipboard.setContents(stringSelection, stringSelection)
          }
        }
        listenTo(mouse.clicks)
      }
      add(new Label("<html><b>" + ver.name + " (" + ver.hostname + ")</b></html>"), "ax center, wrap")
      add(gitCommitLabel, "ax center, wrap")
      add(new Label(ver.database), "span, ax center")
    }
  }

  private val buttonPanel = new MigPanel("insets 0") {
    opaque = false

    def update(buttons:List[PageButton]) {
      removeAll
      buttons.foreach(button => {
        val nb = new NumberedButton(button.name, button.icon, (modifiers) => {
          context.createAndGoTo( (serverContext) => button.pageFactory.create(serverContext), modifiers=modifiers) },
          tooltip0 = button.tooltip)
        add(nb, "sg")
      })
      revalidate()
      repaint()
    }
  }

  val c = new MigPanel("insets 0", "[grow,fill]", "[p]0[grow,fill]") {

    val banner = new MigPanel("insets 0", "[p][p]push[p]") {
      background = Color.WHITE

      val logo = new FixedImagePanel(BrowserIcons.im("/icons/small_sunny_bird3.png"))

      val userImage = BrowserIcons.im("/icons/32x32_user_dark.png")
      val userButton = new NumberedButton(context.localCache.currentFullName, userImage,
        modifiers => {
          val userPages = context.bundles.flatMap(_.userPage(context))
          userPages match {
            case Nil =>
            case page :: Nil => context.goTo(page, modifiers)
            case many => throw new Exception("There is more than one user page: " + many)
          }
        }, false, backgroundColour = Color.WHITE, backgroundOverColour = new Color(0,0,0,32))
      userButton.label.font = new Font("Serif", Font.PLAIN, 20)

      val namePanel = new StarlingNamePanel

      add(logo)
      add(namePanel, "ay center, gapleft 35lp")
      add(userButton, "ay center, gapright 20lp")
    }

    val actionsPanelHolder = new MigPanel("insets 0 " + GuiUtils.StartPageInsets) {
      background = Color.WHITE
      val actionsPanel = new StripedPanel("insets 0", "[grow][p][grow]", "[grow][p][p][grow 150][p]") {

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
        add(buttonPanel, "skip 1, grow, wrap")
        add(bookmarksPanel, "skip 1, growx, wrap")
        add(versionPanel, "newline, split, spanx, ax center, gapbottom 5lp")
      }
      add(actionsPanel, "push,grow")
    }

    add(banner, "wrap")
    add(actionsPanelHolder)
  }

  private def updateBrowserBasedComponents() {
    val homeButtons = context.bundles.flatMap { bundle => { bundle.homeButtons(context) }}
    val utilButtons = context.bundles.flatMap { bundle => { bundle.utilButtons(context) }}
    buttonPanel.update(homeButtons)
    val allButtons = homeButtons /*::: utilButtons
    allButtons.groupBy(_.key).filter(_._1 != None).foreach { case (key, buttons) =>
      Predef.ensuring(buttons.size == 1, "Too many buttons for key " + key.get + " " + buttons)
    }*/
    addButtonActions(allButtons)
    bookmarksPanel.rebuildBookmarks
  }

  reactions += {
    case BundleAdded(_) => updateBrowserBasedComponents()
    case BundleRemoved(_) => updateBrowserBasedComponents()
  }
  listenTo(context.remotePublisher)

  private def addButtonActions(buttons:List[PageButton]) {
    buttons.foreach { button => {
      button.key.foreach { key =>
        val actionName = button.name + "Action"
        peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(key, actionName)
        peer.getActionMap.put(actionName, Action(actionName) {context.createAndGoTo( (sc) => button.pageFactory.create(sc))}.peer)
      }
    }}
  }

  updateBrowserBasedComponents()

  override def defaultComponentForFocus = Some(bookmarksPanel.bookmarksListView.peer)

  private val runAct = bookmarksPanel.goToBookmarkAction1
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_F9, 0), runAct.getValue(javax.swing.Action.NAME))
  peer.getActionMap.put(runAct.getValue(javax.swing.Action.NAME), runAct)


  add(c, "push,grow")

//  context.setDefaultButton(Some(reportsPanel.runButton))
}

case class StarlingHomePageComponentState(bookmarkSelected:Option[BookmarkData]) extends ComponentState

class StarlingNamePanel(addSubTitle:Boolean = true) extends MigPanel("insets 0", rowConstraints = "[p]0[p]") {
  background = Color.WHITE

  val starlingLabel = new Label {
    text = "STARLING"
    font = new Font("Dialog", Font.PLAIN, 64)
    maximumSize = new Dimension(Integer.MAX_VALUE, preferredSize.height-20)

    override protected def paintComponent(g:Graphics2D) {
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      g.drawString(text, 0, size.height - 1)
    }
  }

  val subTitle = if (addSubTitle) {
    StarlingHomePageComponent.subTitle
  } else {
    " "
  }

  val descLabel = new Label(subTitle) {
    font = new Font("Dialog", Font.PLAIN, 14)
    foreground = new Color(0,0,0,128)
    override protected def paintComponent(g:Graphics2D) {
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      g.drawString(text, 0, size.height - g.getFontMetrics.getDescent)
    }
  }

  add(starlingLabel, "wrap")
  add(descLabel, "al right, gapright 5lp")
}