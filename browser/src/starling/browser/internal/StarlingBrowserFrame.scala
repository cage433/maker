package starling.browser.internal

import swing.event.{MouseClicked, WindowClosing}
import swing.Swing._
import java.awt.event.{MouseEvent, MouseAdapter, InputEvent, KeyEvent}
import java.awt.{Color, Dimension}
import javax.swing.{JPopupMenu, JTabbedPane, KeyStroke, JComponent, SwingUtilities}
import swing._
import starling.browser.common.{SXLayerScala, GuiUtils, MigPanel}
import starling.browser.{ServerContext, LocalCache, Page}

class StarlingBrowserFrame(homePage: Page, startPage:Either[Page, (ServerContext => Page, PartialFunction[Throwable, Unit])], pageBuilder: PageBuilder, lCache: LocalCache, userSettings:UserSettings,
                           containerMethods: ContainerMethods, extraInfo:Option[String])
        extends Frame with WindowMethods {
  private val remotePublisher = pageBuilder.remotePublisher
  private val exitAction = Action("Exit") {containerMethods.closeAllFrames(this)}
  peer.getRootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_Q, InputEvent.CTRL_DOWN_MASK), "exitAction")
  peer.getRootPane.getActionMap.put("exitAction", exitAction.peer)

  private val newFrameAction = Action("New Frame") {containerMethods.createNewFrame(Some(this))}
  peer.getRootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_N, InputEvent.CTRL_DOWN_MASK), "newFrameAction")
  peer.getRootPane.getActionMap.put("newFrameAction", newFrameAction.peer)

  private val newFrameAtCurrentPageAction = Action("New Frame At Current Page") {
    containerMethods.createNewFrame(Some(this), startPage = Some(Left(browserTabbedPane.selectedPage)))
  }
  peer.getRootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_N, InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK), "newFrameAtCurrentPageAction")
  peer.getRootPane.getActionMap.put("newFrameAtCurrentPageAction", newFrameAtCurrentPageAction.peer)

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
    containerMethods.updateNotifications()
  }
  peer.getRootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_M, InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK), "fakeMessagesAction")
  peer.getRootPane.getActionMap.put("fakeMessagesAction", fakeMessagesAction.peer)

  private val nineteenInchMode = Action("19 Inch Mode") {size = new Dimension(1280 - 60,1024 - 60)}
  peer.getRootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_D, InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK), "19InchMode")
  peer.getRootPane.getActionMap.put("19InchMode", nineteenInchMode.peer)

  def updateNotifications() {notificationPanel.updateLayout()}

  reactions += {
    case WindowClosing(_) => containerMethods.closeFrame(this)
    case scala.swing.event.UIElementResized(_) => {updateNotifications()}
  }
  listenTo(this)
  private val mainIcon = BrowserIcons.icon("/icons/32x32/status/weather-clear.png").getImage
  private val busyIcon = BrowserIcons.icon("/icons/32x32/status/weather-few-clouds.png").getImage
  iconImage = mainIcon

  private val browserTabbedPane = new StarlingBrowserTabbedPane(homePage, startPage, pageBuilder, lCache, userSettings,
    remotePublisher, this, extraInfo, containerMethods, this)

  private val notificationPanel = new NotificationPanel(size.width, lCache, containerMethods)
  private val mainPanel = new MigPanel("insets 0, hidemode 3", "[p]", "[p]0[p]") {
    background = Color.WHITE
    add(browserTabbedPane, "push, grow, wrap")
    add(notificationPanel, "growx")
  }
  reactions += {
    case MouseClicked(`browserTabbedPane`, _, _, 2, _) => {
      // If the user double clicks to the right of the tab, create a new tab.
      browserTabbedPane.createStarlingBrowser()
    }
  }
  listenTo(browserTabbedPane.mouse.clicks)

  def setBusy(busy: Boolean) {
    if (busy) {
      iconImage = busyIcon
    } else {
      // Check to see if any of the other tabs are busy before saying we are not busy any more.
      if (browserTabbedPane.pages.reverse.tail.filter(_.content.asInstanceOf[SXLayerScala[Component]].getScalaComponent.asInstanceOf[StarlingBrowser].busy).isEmpty) {
        iconImage = mainIcon
      }
    }
  }

  def canClose = (browserTabbedPane.pages.size > 2)

  def setDefaultButton(button: Option[Button]) {
    defaultButton = button
  }

  def getDefaultButton = defaultButton

  def containsPage(page:Page) = {
    browserTabbedPane.pages.find(p => {
      p.content match {
        case c:SXLayerScala[_] => c.asInstanceOf[SXLayerScala[StarlingBrowser]].getScalaComponent.currentPage match {
          case Some(cp) => cp == page
          case None => false
        }
        case _ => false
      }
    }) match {
      case Some(_) => true
      case _ => false
    }
  }

  def showNewPage(page:Page) {browserTabbedPane.createStarlingBrowser(true, Left(page))}
  def showPage(page:Page) {
    browserTabbedPane.selectPage(page)
  }

  contents = mainPanel
}

trait WindowMethods {
  def setBusy(busy: Boolean)
  def canClose:Boolean
  def setDefaultButton(button:Option[Button])
  def getDefaultButton:Option[Button]
}

class StarlingBrowserTabbedPane(homePage: Page, startPage:Either[Page,(ServerContext => Page, PartialFunction[Throwable,Unit])], pageBuilder: PageBuilder, lCache: LocalCache, userSettings:UserSettings,
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
      if (SwingUtilities.isRightMouseButton(e)) {
        tabPopupMenu.show(e.getComponent, e.getX, e.getY)
      } else {
        selection.page.content.requestFocusInWindow()
      }
    }
  })
  background = new Color(164,201,246) // Set this for deselected tabs.

  private val newTabAction = Action("New Tab") {createStarlingBrowser()}
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_T, InputEvent.CTRL_DOWN_MASK), "newTabAction")
  peer.getActionMap.put("newTabAction", newTabAction.peer)

  private val newTabAtCurrentPageAction = Action("New Tab At Current Page") {createStarlingBrowser(pageOrBuildPage = Left(selectedPage))}
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_T, InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK), "newTabAtCurrentPageAction")
  peer.getActionMap.put("newTabAtCurrentPageAction", newTabAtCurrentPageAction.peer)

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
      val browser = pages(newSelection).self.asInstanceOf[SXLayerScala[StarlingBrowser]].getScalaComponent
      browser.selected()
    } else {
      containerMethods.closeFrame(parentFrame)
    }
  }
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_F4, InputEvent.CTRL_DOWN_MASK), "closeTabAction")
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_W, InputEvent.CTRL_DOWN_MASK), "closeTabAction")
  peer.getActionMap.put("closeTabAction", closeTabAction.peer)

  private val newTabItem = new MenuItem(newTabAction)
  private val closeTabItem = new MenuItem(closeTabAction)
  private val tabPopupMenu = new JPopupMenu
  tabPopupMenu.setBorder(LineBorder(GuiUtils.BorderColour))
  tabPopupMenu.add(newTabItem.peer)
  tabPopupMenu.addSeparator()
  tabPopupMenu.add(closeTabItem.peer)

  def createStarlingBrowser(gotoTab: Boolean = true, pageOrBuildPage: Either[Page, (ServerContext => Page, PartialFunction[Throwable, Unit])] = Left(homePage)): StarlingBrowser = {
    val (tabText, icon, addressText) = pageOrBuildPage match {
      case Left(page) => (page.shortText, page.icon, page.text)
      case _ => (" ", BrowserIcons.im("/icons/10x10_blank.png"), " ")
    }
    val tabComponent = new TabComponent(windowMethods, this, tabText, icon)
    val starlingBrowser = new StarlingBrowser(pageBuilder, lCache, userSettings, homePage, addressText,
      windowMethods, containerMethods, parentFrame, tabComponent, createStarlingBrowser, extraInfo)
    pages.insert(pages.length - 1, new scala.swing.TabbedPane.Page(tabText, starlingBrowser.starlingBrowserLayer, null))
    val tabIndex = if (pages.length == 0) 0 else pages.length - 2
    if (gotoTab) {
      selection.index = tabIndex
    }
    peer.setTabComponentAt(tabIndex, tabComponent.peer)
    onEDT(starlingBrowser.goTo(pageOrBuildPage, true))
    starlingBrowser
  }

  def selectedPage:Page = {
    selection.page.content match {
      case c:SXLayerScala[_] => c.asInstanceOf[SXLayerScala[StarlingBrowser]].getScalaComponent.currentPage match {
        case Some(cp) => cp
        case None => throw new Exception("I would always expect a page to be available at this point")
      }
    }
  }

  def selectPage(page:Page) {
    val (_, i) = pages.zipWithIndex.find{case (p, _) => {
      p.content match {
        case c:SXLayerScala[_] => c.asInstanceOf[SXLayerScala[StarlingBrowser]].getScalaComponent.currentPage match {
          case Some(cp) => cp == page
          case None => false
        }
      }
    }}.get
    selection.index = i
  }

  // We want an new tab button.
  pages += new scala.swing.TabbedPane.Page("", new Label("Blank Tab"), "Open a new tab")
  peer.setTabComponentAt(0, TabComponent.createNewTabComponent(createStarlingBrowser()).peer)
  peer.setEnabledAt(0, false)

  // This is run for the side effects. That's a bit naughty!
  createStarlingBrowser(pageOrBuildPage = startPage)
}
