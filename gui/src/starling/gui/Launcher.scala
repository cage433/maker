package starling.gui

import api._
import javax.swing._
import pages.{HelpPage, StarlingHomePage, PageBuilder, StarlingBrowser}
import scala.swing.event.WindowClosing
import scala.swing.event.MouseClicked
import scala.swing.event.ButtonClicked
import starling.gui.StandardUserSettingKeys._
import swing._
import scala.swing.Action
import java.awt.event._
import scala.swing.Swing._
import starling.rmi.{StarlingServer}
import collection.mutable.ListBuffer
import starling.bouncyrmi.{BouncyRMIClient}
import javax.security.auth.login.LoginException
import starling.pivot.view.swing.{FixedImagePanel, MigPanel, SXLayerScala}
import starling.utils.{StackTraceToString, Log, HeterogeneousMap}
import starling.gui.LocalCacheKeys._
import starling.daterange.Day
import starling.auth.{Client, ClientLogin}
import management.ManagementFactory
import java.awt.{GraphicsEnvironment, Color, KeyboardFocusManager}
import xstream.GuiStarlingXStream

/**
 * The entry point into the starling gui
 */
object Launcher {
  def main(args: Array[String]) {
    if (args.length != 3) {
      throw new IllegalArgumentException("You need to specify 3 arguments: hostname, rmi port and servicePrincipalName")
    }
    println(List() ++ args)
    val rmiHost = args(0)
    val rmiPort = args(1).toInt
    val servicePrincipalName = args(2)
    start(rmiHost, rmiPort, servicePrincipalName)
  }

  def systemInfo = {
    import starling.utils.ImplicitConversions._

    val screenDevices = GraphicsEnvironment.getLocalGraphicsEnvironment.getScreenDevices.toList
    val monitors = screenDevices.map(sd => {
      val dm = sd.getDisplayMode

      MonitorInfo(
        sd.toString,
        dm.getWidth,
        dm.getHeight,
        sd.getAvailableAcceleratedMemory.megs
      )
    })

    val system = ManagementFactory.getOperatingSystemMXBean
    val vm = ManagementFactory.getRuntimeMXBean

    val (totalMem, freeMem) = system match {
      case sun:com.sun.management.OperatingSystemMXBean => (sun.getTotalPhysicalMemorySize.megs.toInt, sun.getFreePhysicalMemorySize.megs.toInt)
      case _ => (-1,-1)
    }

    OSInfo(
      system.getName,
      system.getArch,
      system.getVersion,
      vm.getVmName,
      vm.getVmVendor,
      System.getProperty("java.runtime.version"),
      system.getAvailableProcessors,
      totalMem,
      freeMem,
      monitors.size,
      monitors
    )
  }

  // These variables are a big hack so we remember what they are when running the start method when changing users.
  var rmiHost = ""
  var rmiPort = -1
  var servicePrincipalName = ""
  var numberOfClientsLaunched = 0

  def start(rmiHost: String, rmiPort: Int, servicePrincipalName: String, overriddenUser:Option[String] = None) {
    this.rmiHost = rmiHost
    this.rmiPort = rmiPort
    this.servicePrincipalName = servicePrincipalName
    numberOfClientsLaunched += 1

    def logger(message:String) {
      // TODO [03 Feb 2011] do something clever with this message.
//      println(message)
    }
    val client = new BouncyRMIClient(rmiHost, rmiPort, classOf[StarlingServer], auth(servicePrincipalName), logger, overriddenUser)
    try {
      client.startBlocking
      val starlingServer = client.proxy
      val extraInfo = overriddenUser match {
        case None => {
          // When the user isn't overridden, store the system info on each log on.
          starlingServer.storeSystemInfo(systemInfo)
          None
        }
        case Some(_) => Some("You are " + starlingServer.whoAmI.name) // Want to see who I actually am, not who I tried to be.
      }
      start(starlingServer, client.remotePublisher, new StarlingHomePage, extraInfo)
    }
    catch {
      case t => showErrorThenExit(t)
    }
  }

  private def getMessage(t: Throwable): String = {
    val m = if (t.getMessage == null) {
      "Error"
    } else {
      t.getMessage
    }

    if (m.length > 60) {
      m.substring(0, 60) + " ... "
    } else {
      m
    }

    m
  }

  def showErrorThenExit(t: Throwable) {
    Log.fatal("Failed to start starling: ", t)
    onEDT {
      GuiUtils.setLookAndFeel
      val f = new Frame {
        title = "Could not start Starling"
        iconImage = StarlingIcons.icon("/icons/32x32/status/weather-few-clouds.png").getImage
        val okButton = new Button("Ok") {reactions += {case ButtonClicked(e) => exit}}
        contents = new MigPanel("insets n 0 n n") {
          val image = StarlingIcons.im("/icons/128x128_storm_dead_bird.png")
          val imagePanel = new FixedImagePanel(image)

          val message = getMessage(t)

          val l = new Label("Failed to start Starling: " + message)

          val textArea = new TextArea(StackTraceToString.string(t)) {
            editable = false
          }
          val scrollPane = new ScrollPane(textArea) {
            preferredSize = new Dimension(500, 200)
          }

          add(imagePanel, "ay top")
          add(l, "ay top, gaptop 40lp, split, flowy")
          add(scrollPane, "gaptop 30lp, wrap unrel, push, grow")
          add(okButton, "split, spanx, tag ok")
        }
        pack
        centerOnScreen
        defaultButton = okButton
        visible = true

        reactions += {
          case WindowClosing(w) => exit
        }

        def exit = System.exit(-1)
      }
    }
  }

  def auth(servicePrincipalName: String): Client = {
    try {
      val subject = new ClientLogin().login
      new Client(servicePrincipalName, subject)
    } catch {
      case l: LoginException => {
        import starling.utils.Utils._
        os match {
          case Linux => {
            Log.error("Failed to initialise kerberos, either it isn't used on this system or the ticket cache is stale (try krenew). Skipping kerberos.")
            new Client(null, null) {
              override def ticket = null
            }
          }
          case _: Windows => {
            throw new Exception("Windows: Failed to initialise kerberos for Starling log in.", l)
          }
          case u: UnknownOS => {
            throw new Exception(u + ": Failed to initialise kerberos for Starling log in.", l)
          }
        }
      }
    }
  }

  def start(starlingServer: StarlingServer, remotePublisher: Publisher, homePage: Page, extraInfo:Option[String]) {
    onEDT({
      val title = starlingServer.name + " - Starling"

      val localCacheUpdatePublisher = new scala.swing.Publisher() {}
      val postLocalCacheUpdatePublisher = new scala.swing.Publisher() {}

      val settings = starlingServer.readSettings

      remotePublisher.reactions += {
        case batch:EventBatch => {
          onEDT {
            //This indirection between the remotePublisher ensures that pages see
            // the events after the local cache is updated
            // and also ensures that the pages receive the event on the EDT
            batch.events.foreach { e => localCacheUpdatePublisher.publish(e) }
            postLocalCacheUpdatePublisher.publish(batch)
          }
        }
      }

      val cacheMap = new HeterogeneousMap[Key]
      val cache = LocalCache(cacheMap)
      lazy val fc = new StarlingBrowserFrameContainer(starlingServer, cache, postLocalCacheUpdatePublisher, homePage,
        settings, title, extraInfo)

      // Must be called on the EDT
      def sendNotification(notification:Notification) {
        cacheMap(AllNotifications) = notification :: cacheMap(AllNotifications)
        cacheMap(UserNotifications) = notification :: cacheMap(UserNotifications)
        fc.updateNotifications
      }

      val username = starlingServer.whoAmI.username

      def toBookmarks(labels:List[BookmarkLabel]) = {
        labels.map(s => {
          val bookmark = GuiStarlingXStream.read(s.bookmark).asInstanceOf[Bookmark]
          val bookmarkName = s.name
          BookmarkData(bookmarkName, bookmark)
        })
      }

      // We want to set up a local cache so that we don't always have to hit the server. This cache is populated on startup and then listens to
      // the publisher to stay fresh.
      localCacheUpdatePublisher.reactions += {
        case e: PivotLayoutUpdate if (e.user == username) => {
          cacheMap(UserPivotLayouts) = e.userLayouts
        }
        case e: BookmarksUpdate if e.user == username => {
          cacheMap(Bookmarks) = toBookmarks(e.bookmarks)
        }
        case ExcelMarketListUpdate(values) => {
          cacheMap(ExcelDataSets) = values
        }
        case MarketDataSnapshot(snapshots) => {
          cacheMap(Snapshots) = snapshots
        }
        case PricingGroupMarketDataUpdate(pg, version) => {
          cacheMap(PricingGroupLatestMarketDataVersion) =
                  cacheMap(PricingGroupLatestMarketDataVersion) + (pg -> version)
        }
        case ExcelMarketDataUpdate(excel, version) => {
          cacheMap(ExcelLatestMarketDataVersion) =
                  cacheMap(ExcelLatestMarketDataVersion) + (excel -> version)
        }
        case DeskClosed(desk, timestamp) => {
          if(cache.desks.contains(desk)) {
            val old: Map[Desk, Map[Day, List[TradeTimestamp]]] = cacheMap(DeskCloses)
            val oldCloses: Map[Day, List[TradeTimestamp]] = old.getOrElse(desk, Map())
            val newCloses: Map[Day, List[TradeTimestamp]] = oldCloses + (timestamp.closeDay -> (timestamp :: oldCloses.getOrElse(timestamp.closeDay, Nil)))
            val newMap = old + (desk -> newCloses)
            cacheMap(DeskCloses) = newMap
            val text = "Imported book close for " + desk + " (" + timestamp.timestamp.toStringMinutes + ")"
            val notification =  Notification(text, StarlingIcons.icon("/icons/16x16_book.png"), NotificationType.Message, {})
            sendNotification(notification)
          }
        }
        case DeskCloseFailed(desk, timestamp, error) => {
          if(cache.desks.contains(desk)) {
            val old: Map[Desk, Map[Day, List[TradeTimestamp]]] = cacheMap(DeskCloses)
            val oldCloses: Map[Day, List[TradeTimestamp]] = old.getOrElse(desk, Map())
            val newCloses: Map[Day, List[TradeTimestamp]] = oldCloses + (timestamp.closeDay -> (timestamp :: oldCloses.getOrElse(timestamp.closeDay, Nil)))
            val newMap = old + (desk -> newCloses)
            cacheMap(DeskCloses) = newMap
            val text = "Import failed for " + desk + " (" + timestamp.timestamp.toStringMinutes + ")"
            val notification = Notification(text, StarlingIcons.icon("/icons/16x16_error.png"), NotificationType.Message, {})
            sendNotification(notification)
          }
        }
        case IntradayUpdated(group, user, timestamp) => {
          cacheMap(IntradayLatest) = cacheMap(IntradayLatest) + (group -> (user, timestamp))
        }
        case ExcelObservationDay(name, day) => {
          val current = cacheMap(ObservationDaysForExcel)
          cacheMap(ObservationDaysForExcel) = {
            current.updated(name, current.getOrElse(name, Set()) + day)
          }
        }
        case PricingGroupObservationDay(pricingGroup, day) => {
          val current = cacheMap(ObservationDaysForPricingGroup)
          cacheMap(ObservationDaysForPricingGroup) = {
            current.updated(pricingGroup, current.getOrElse(pricingGroup, Set()) + day)
          }
        }
        case UserLoggedIn(user) => {
          val current = cacheMap(AllUserNames)
          if (!current.exists(_ == user.username)) {
            cacheMap(AllUserNames) = user.username :: current
          }
        }
      }

      try {
        cacheMap(AllUserNames) = starlingServer.allUserNames
        cacheMap(UserPivotLayouts) = starlingServer.extraLayouts
        cacheMap(PricingGroups) = starlingServer.pricingGroups
        cacheMap(ExcelDataSets) = starlingServer.excelDataSets
        cacheMap(Snapshots) = starlingServer.snapshots
        val (observationDaysForPricingGroup, observationDaysForExcel) = starlingServer.observationDays
        cacheMap(ObservationDaysForPricingGroup) = observationDaysForPricingGroup
        cacheMap(ObservationDaysForExcel) = observationDaysForExcel
        cacheMap(ExcelLatestMarketDataVersion) = starlingServer.excelLatestMarketDataVersions
        cacheMap(PricingGroupLatestMarketDataVersion) = starlingServer.pricingGroupLatestMarketDataVersions
        cacheMap(LocalCacheKeys.ReportOptionsAvailable) = starlingServer.reportOptionsAvailable
        cacheMap(AllNotifications) = List()
        cacheMap(UserNotifications) = List()
        cacheMap(LocalCacheKeys.Version) = starlingServer.version
        cacheMap(DeskCloses) = starlingServer.deskCloses
        cacheMap(IntradayLatest) = starlingServer.intradayLatest
        cacheMap(TradersBookLookup) = starlingServer.traders
        cacheMap(CurrentUser) = starlingServer.whoAmI
        cacheMap(UKBusinessCalendar) = starlingServer.ukBusinessCalendar
        cacheMap(Desks) = starlingServer.desks
        cacheMap(GroupToDesksMap) = starlingServer.groupToDesksMap
        cacheMap(IsStarlingDeveloper) = starlingServer.isStarlingDeveloper
        cacheMap(EnvironmentRules) = starlingServer.environmentRules
        cacheMap(CurveTypes) = starlingServer.curveTypes
        cacheMap(Bookmarks) = toBookmarks(starlingServer.bookmarks)

        GuiUtils.setLookAndFeel
      } catch {
        case e : Throwable =>
          e.printStackTrace()
          throw e
      }

      fc
    })    
  }
}

trait ContainerMethods {
  def createNewFrame(fromFrame: Option[StarlingBrowserFrame])
  def closeFrame(frame: StarlingBrowserFrame)
  def closeAllFrames(fromFrame: StarlingBrowserFrame)
  def updateNotifications
}

class StarlingBrowserFrameContainer(starlingServer: StarlingServer, lCache: LocalCache, remotePublisher: Publisher,
                                    homePage: Page, userSettings: UserSettings, frameTitle: String, extraInfo:Option[String]) extends ContainerMethods {
  private val pageBuilder = new PageBuilder(new PageBuildingContext(starlingServer))
  private val frames = new ListBuffer[StarlingBrowserFrame]

  def createNewFrame(fromFrame: Option[StarlingBrowserFrame]) = {
    val newFrame = new StarlingBrowserFrame(homePage, pageBuilder, lCache, userSettings, remotePublisher, this, extraInfo)
    frames += newFrame
    newFrame.title = frameTitle
    fromFrame match {
      case None => {
        // Load the user settings.
        if (userSettings.settingExists(MainWindowBounds)) {
          // Need to check if the screen is on the screen.
          val frameBounds = userSettings.getSetting(MainWindowBounds)
          if (GuiUtils.onScreen(frameBounds)) {
            newFrame.peer.setBounds(frameBounds)
          } else {
            newFrame.pack
            newFrame.centerOnScreen
          }
        } else {
          newFrame.pack
          newFrame.centerOnScreen
        }
      }
      case Some(fFrame) => {
        // Display the new frame slightly below the other frame, with the same size.
        newFrame.peer.setBounds(fFrame.bounds.x + 20, fFrame.bounds.y + 20, fFrame.bounds.width, fFrame.bounds.height)
      }
    }
    newFrame.visible = true
  }

  private def saveUserSettings(frame: Frame) {
    try {
      userSettings.putSetting(MainWindowBounds, frame.bounds)
      for (f <- frames) f.visible = false
      starlingServer.saveSettings(userSettings)
    } catch {
      case e => println("Exception whilst saving user userSettings\n" + e)
    }
  }

  def closeFrame(frame: StarlingBrowserFrame) = {
    if ((frames.size == 1) && (Launcher.numberOfClientsLaunched <= 1)) {
      // This is the last frame so ensure user userSettings are saved.
      closeAllFrames(frame)
    } else {
      // There are other frames left so just dispose of this frame.
      Launcher.numberOfClientsLaunched -= 1
      frames -= frame
      frame.visible = false
      frame.dispose
    }
  }

  def closeAllFrames(fromFrame: StarlingBrowserFrame) = {
    println("Shutting down")
    saveUserSettings(fromFrame)
    System.exit(0)
  }

  def updateNotifications = {
    frames.foreach(_.updateNotifications)
  }

  createNewFrame(None)
}

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
    val notification =  Notification(text, StarlingIcons.icon("/icons/16x16_book.png"), NotificationType.Action, {println(text)})

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
  private val mainIcon = StarlingIcons.icon("/icons/32x32/status/weather-clear.png").getImage
  private val busyIcon = StarlingIcons.icon("/icons/32x32/status/weather-few-clouds.png").getImage
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
    icon = StarlingIcons.icon("/icons/splitVertically.png")
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

  def createStarlingBrowser(gotoTab: Boolean = true, pageOrBuildPage: Either[Page, (StarlingServer => Page, PartialFunction[Throwable, Unit])] = Left(homePage)): StarlingBrowser = {
    val (tabText, icon, addressText) = pageOrBuildPage match {
      case Left(page) => (page.shortText, page.icon, page.text)
      case _ => (" ", StarlingIcons.im("/icons/10x10_blank.png"), " ")
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
