package starling.browser.internal

import swing.Action._
import swing.UIElement._
import swing.event.{MouseClicked, WindowClosing}
import collection.mutable.ListBuffer
import swing.Swing._
import swing.Component._
import java.awt.event.{MouseEvent, MouseAdapter, InputEvent, KeyEvent}
import java.awt.{Color, Dimension}
import javax.swing.{JPopupMenu, JTabbedPane, KeyStroke, JComponent}
import swing._
import starling.browser.common.{SXLayerScala, GuiUtils, MigPanel}
import starling.browser.{BrowserBundle, ServerContext, LocalCache, Page}

class StarlingBrowserFrame(homePage: Page, pageBuilder: PageBuilder, lCache: LocalCache, userSettings:UserSettings,
                           remotePublisher: Publisher, containerMethods: ContainerMethods, extraInfo:Option[String])
        extends Frame with WindowMethods {
  private val exitAction = Action("Exit") {containerMethods.closeAllFrames(this)}
  peer.getRootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_Q, InputEvent.CTRL_DOWN_MASK), "exitAction")
  peer.getRootPane.getActionMap.put("exitAction", exitAction.peer)

  private val newFrameAction = Action("New Frame") {containerMethods.createNewFrame(Some(this))}
  peer.getRootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_N, InputEvent.CTRL_DOWN_MASK), "newFrameAction")
  peer.getRootPane.getActionMap.put("newFrameAction", newFrameAction.peer)

  private val showMessagesAction = Action("Show Messages") {
    notificationPanel.visible = !notificationPanel.visible
  }
  peer.getRootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_M, InputEvent.CTRL_DOWN_MASK), "showMessagesAction")
  peer.getRootPane.getActionMap.put("showMessagesAction", showMessagesAction.peer)

  private val fakeMessagesAction = Action("Fake Message") {
    val text = "This is a notification " + new java.util.Random().nextInt
    val notification =  Notification(text, BrowserIcons.icon("/icons/16x16_book.png"), NotificationType.Action, {println(text)})

    import NotificationKeys._
    lCache.localCache(AllNotifications) = notification :: lCache.localCache(AllNotifications)
    lCache.localCache(UserNotifications) = notification :: lCache.localCache(UserNotifications)
    containerMethods.updateNotifications
  }
  peer.getRootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_M, InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK), "fakeMessagesAction")
  peer.getRootPane.getActionMap.put("fakeMessagesAction", fakeMessagesAction.peer)

  private val closeFrameAction = Action("Close Window") {containerMethods.closeFrame(this)}
//  peer.getRootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_W, InputEvent.CTRL_DOWN_MASK), "closeFrameAction")
  peer.getRootPane.getRootPane.getActionMap.put("closeFrameAction", closeFrameAction.peer)

  private val nineteenInchMode = Action("19 Inch Mode") {size = new Dimension(1280 - 60,1024 - 60)}
  peer.getRootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_N, InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK), "19InchMode")
  peer.getRootPane.getActionMap.put("19InchMode", nineteenInchMode.peer)

  def updateNotifications = notificationPanel.updateLayout

  reactions += {
    case WindowClosing(_) => containerMethods.closeFrame(this)
    case scala.swing.event.UIElementResized(_) => {updateNotifications}
  }
  listenTo(this)
  private val mainIcon = BrowserIcons.icon("/icons/32x32/status/weather-clear.png").getImage
  private val busyIcon = BrowserIcons.icon("/icons/32x32/status/weather-few-clouds.png").getImage
  iconImage = mainIcon

  private val initialStarlingBrowser = new StarlingBrowserTabbedPane(homePage, pageBuilder, lCache, userSettings,
    remotePublisher, this, extraInfo, containerMethods, this)

  private val notificationPanel = new NotificationPanel(size.width, lCache, containerMethods)
  private val mainPanel = new MigPanel("insets 0, hidemode 3", "[p]", "[p]0[p]") {
    //    background = new Color(91,139,216)
    //    background = Color.WHITE
    def setNewComponent(comp:Component) {
      removeAll
      add(comp, "push, grow, wrap")
      add(notificationPanel, "growx")
      revalidate
      repaint
    }
  }
  reactions += {
    case MouseClicked(`initialStarlingBrowser`, _, _, 2, _) => {
      // If the user double clicks to the right of the tab, create a new tab.
      initialStarlingBrowser.createStarlingBrowser()
    }
  }
  listenTo(initialStarlingBrowser.mouse.clicks)

  private val tabbedPaneBuffer = new ListBuffer[StarlingBrowserTabbedPane]
  tabbedPaneBuffer += initialStarlingBrowser
  regenerateFromBuffer(initialStarlingBrowser.pages(0).content)

  def splitVertically(eventFrom: StarlingBrowserTabbedPane) = {
    val starlingBrowserTabbedPane = new StarlingBrowserTabbedPane(homePage, pageBuilder, lCache, userSettings,
      remotePublisher, this, extraInfo, containerMethods, this)
    reactions += {
      case MouseClicked(`starlingBrowserTabbedPane`, _, _, 2, _) => {
        // If the user double clicks to the right of the tab, create a new tab.
        starlingBrowserTabbedPane.createStarlingBrowser()
      }
    }
    listenTo(starlingBrowserTabbedPane.mouse.clicks)
    val indexToAdd = tabbedPaneBuffer.indexOf(eventFrom) + 1
    tabbedPaneBuffer.insert(indexToAdd, starlingBrowserTabbedPane)
    regenerateFromBuffer(starlingBrowserTabbedPane.pages(0).content)
  }

  def setBusy(busy: Boolean) = {
    if (busy) {
      iconImage = busyIcon
    } else {
      // Check to see if any of the other tabs are busy before saying we are not busy any more.
      if (tabbedPaneBuffer.flatMap(_.pages.reverse.tail.filter(_.content.asInstanceOf[SXLayerScala[Component]].getScalaComponent.asInstanceOf[StarlingBrowser].busy)).isEmpty) {
        iconImage = mainIcon
      }
    }
  }

  def canClose = ((for (starlingTabbedPane <- tabbedPaneBuffer) yield starlingTabbedPane.pages.length).sum > 2)

  def tabClosed() {
    val starlingTabbedPanesToRemove = tabbedPaneBuffer.filter(_.pages.length == 1)
    if (starlingTabbedPanesToRemove.size > 0) {
      tabbedPaneBuffer --= starlingTabbedPanesToRemove
      regenerateFromBuffer()
    }
  }

  private def regenerateFromBuffer(focusComponent: Component = tabbedPaneBuffer.last.selection.page.content) {
    // TODO [16 Apr 2010] ensure the correct size is used for the components in the split panes.
    val componentToAdd = tabbedPaneBuffer.reduceRight[Component](new SplitPane(Orientation.Vertical, _, _) {
      border = EmptyBorder
    })
    mainPanel.setNewComponent(componentToAdd)
  }

  def setDefaultButton(button: Option[Button]) {
    defaultButton = button
  }

  def getDefaultButton = defaultButton

  contents = mainPanel
}

trait WindowMethods {
  def setBusy(busy: Boolean)
  def splitVertically(eventFrom: StarlingBrowserTabbedPane)
  def canClose:Boolean
  def tabClosed()
  def setDefaultButton(button:Option[Button])
  def getDefaultButton:Option[Button]
}

class StarlingBrowserTabbedPane(homePage: Page, pageBuilder: PageBuilder, lCache: LocalCache, userSettings:UserSettings,
                                remotePublisher: Publisher, windowMethods: WindowMethods, extraInfo:Option[String],
                                containerMethods:ContainerMethods, parentFrame:StarlingBrowserFrame) extends TabbedPane {
  override lazy val peer = new JTabbedPane with SuperMixin {
    override def setSelectedIndex(index:Int) {
      if (selection.index != -1) {
        selection.page.content match {
          case c:SXLayerScala[_] => c.asInstanceOf[SXLayerScala[StarlingBrowser]].getScalaComponent.unselected()
          case _ =>
        }
      }

      super.setSelectedIndex(index)

      // Tell the current page it has been selected.
      selection.page.content match {
        case c:SXLayerScala[_] => c.asInstanceOf[SXLayerScala[StarlingBrowser]].getScalaComponent.selected()
        case _ =>
      }
    }
  }

  focusable = false
  peer.setUI(new StarlingTabbedPaneUI)
  peer.addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent) {
      if (e.isPopupTrigger) {
        tabPopupMenu.show(e.getComponent, e.getX, e.getY)
      } else {
        selection.page.content.requestFocusInWindow()
      }
    }

    override def mouseReleased(e: MouseEvent) {
      if (e.isPopupTrigger) {
        tabPopupMenu.show(e.getComponent, e.getX, e.getY)
      }
    }
  })
//  background = new Color(171, 206, 247) // Set this for deselected tabs.
  background = new Color(164,201,246) // Set this for deselected tabs.

  private val newTabAction = Action("New Tab") {createStarlingBrowser()}
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_T, InputEvent.CTRL_DOWN_MASK), "newTabAction")
  peer.getActionMap.put("newTabAction", newTabAction.peer)

  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0), "helpAction")
  peer.getActionMap.put("helpAction", Action("helpAction"){createStarlingBrowser(true, Left(HelpPage))}.peer)

  private val nextTabAction = Action("Next Tab") {
    val nextIndex = selection.index + 1
    if (nextIndex < pages.length - 1) {
      selection.index = nextIndex
    } else {
      selection.index = 0
    }
  }
  private val previousTabAction = Action("Previous Tab") {
    val previousIndex = selection.index - 1
    if (previousIndex < 0) {
      selection.index = pages.length - 2
    } else {
      selection.index = previousIndex
    }
  }
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP, InputEvent.CTRL_DOWN_MASK), "previousTabAction")
  peer.getActionMap.put("previousTabAction", previousTabAction.peer)
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN, InputEvent.CTRL_DOWN_MASK), "nextTabAction")
  peer.getActionMap.put("nextTabAction", nextTabAction.peer)

  private val closeTabAction = Action("Close Tab") {
    if (windowMethods.canClose) {
      val selectionToRemove = selection.index
      pages.remove(selectionToRemove)
      val newSelection = if (selectionToRemove == pages.length - 1) selectionToRemove - 1 else selectionToRemove
      selection.index = newSelection
      windowMethods.tabClosed()
      val browser = pages(newSelection).self.asInstanceOf[SXLayerScala[StarlingBrowser]].getScalaComponent
      browser.selected()
    } else {
      containerMethods.closeFrame(parentFrame)
    }
  }
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_F4, InputEvent.CTRL_DOWN_MASK), "closeTabAction")
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_W, InputEvent.CTRL_DOWN_MASK), "closeTabAction")
  peer.getActionMap.put("closeTabAction", closeTabAction.peer)

  private val splitVerticallyAction = new Action("Split Vertically") {
    icon = BrowserIcons.icon("/icons/splitVertically.png")
    def apply() {
      windowMethods.splitVertically(StarlingBrowserTabbedPane.this)
    }
  }

  private val newTabItem = new MenuItem(newTabAction)
  private val closeTabItem = new MenuItem(closeTabAction)
  private val splitVerticallyItem = new MenuItem(splitVerticallyAction)
  private val tabPopupMenu = new JPopupMenu
  tabPopupMenu.setBorder(LineBorder(GuiUtils.BorderColour))
  tabPopupMenu.add(newTabItem.peer)
  tabPopupMenu.addSeparator()
  tabPopupMenu.add(closeTabItem.peer)
  tabPopupMenu.addSeparator()
  tabPopupMenu.add(splitVerticallyItem.peer)

  def createStarlingBrowser(gotoTab: Boolean = true, pageOrBuildPage: Either[Page, (ServerContext => Page, PartialFunction[Throwable, Unit])] = Left(homePage)): StarlingBrowser = {
    val (tabText, icon, addressText) = pageOrBuildPage match {
      case Left(page) => (page.shortText, page.icon, page.text)
      case _ => (" ", BrowserIcons.im("/icons/10x10_blank.png"), " ")
    }
    val tabComponent = new TabComponent(windowMethods, this, tabText, icon)
    val starlingBrowser = new StarlingBrowser(pageBuilder, lCache, userSettings, remotePublisher, homePage, addressText,
      windowMethods, tabComponent, createStarlingBrowser, extraInfo)
    pages.insert(pages.length - 1, new scala.swing.TabbedPane.Page(tabText, starlingBrowser.starlingBrowserLayer, null))
    val tabIndex = if (pages.length == 0) 0 else pages.length - 2
    if (gotoTab) {
      selection.index = tabIndex
    }
    peer.setTabComponentAt(tabIndex, tabComponent.peer)
    onEDT(starlingBrowser.goTo(pageOrBuildPage, true))
    starlingBrowser
  }

  // We want an new tab button.
  pages += new scala.swing.TabbedPane.Page("", new Label("Blank Tab"), "Open a new tab")
  peer.setTabComponentAt(0, TabComponent.createNewTabComponent(createStarlingBrowser()).peer)
  peer.setEnabledAt(0, false)

  // This is run for the side effects. That's a bit naughty!
  createStarlingBrowser()
}
