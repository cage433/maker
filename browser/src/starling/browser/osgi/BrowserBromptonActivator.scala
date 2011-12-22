package starling.browser.osgi

import starling.browser.service.internal.HeterogeneousMap
import starling.browser.service._
import starling.browser.internal.HomePage.StarlingHomePage
import starling.browser.internal._
import com.jgoodies.looks.plastic.PlasticXPLookAndFeel
import swing.Swing._
import starling.browser.common.GuiUtils
import starling.browser._
import swing.event.Event
import java.io.{File, FileInputStream}
import java.util.{Hashtable, Properties}
import starling.manager._
import management.ManagementFactory
import javax.swing.{SwingUtilities, UIManager}
import java.util.concurrent.CountDownLatch
import swing.{Swing, Publisher}
import java.util.concurrent.atomic.AtomicInteger

case class RunAsUser(name:Option[String])

case class BundleAdded(bundle:BrowserBundle) extends StarlingGUIEvent
case class BundleRemoved(bundle:BrowserBundle) extends StarlingGUIEvent

class IncrementingCountDownLatch(initialCount:Int) {
  val counter = new java.util.concurrent.atomic.AtomicInteger(1)
  def awaitZero {

    while(counter.get != 0) {
      counter.synchronized{counter.wait()}
    }
  }
  def increment() {
    counter.incrementAndGet()
  }
  def decrement() {
    counter.decrementAndGet()
    counter.synchronized{ counter.notify()}
  }
}

class BrowserBromptonActivator extends BromptonActivator {

  def start(context: BromptonContext) {

    val bundlesByRef = new scala.collection.mutable.HashMap[BromptonServiceReference,(BrowserBundle,Publisher)]()
    val bundlesByName = new scala.collection.mutable.HashMap[String,BrowserBundle]()
    val cacheMap = new HeterogeneousMap[LocalCacheKey]()
    val cache = LocalCache(cacheMap)
    cacheMap(NotificationKeys.AllNotifications) = List()
    cacheMap(NotificationKeys.UserNotifications) = List()
    context.registerService(classOf[LocalCache], cache)
    val localCachePublisher = new Publisher() {}
    val pageContextPublisher = new Publisher() {}
    var fc:StarlingBrowserFrameContainer = null

    val latch = new IncrementingCountDownLatch(1)
    val settings = new UserSettings(pageContextPublisher)
    val overriddenUser = context.awaitService(classOf[RunAsUser])

    javax.swing.SwingUtilities.invokeLater(new Runnable() { def run() {
      UIManager.getDefaults.put("ClassLoader", classOf[PlasticXPLookAndFeel].getClassLoader)
      GuiUtils.setLookAndFeel()

      val serverContext = new ServerContext() {
        def lookup[T](klass: Class[T]) = context.awaitService(klass)
        def browserService = context.awaitService(classOf[BrowserService])
      }
      val pageBuilder = new PageBuilder(pageContextPublisher, serverContext, bundlesByName)


      def returnHomePageWhenReady(serverContext:ServerContext) = {
        latch.awaitZero
        StarlingHomePage
      }

      val appName = System.getProperty("appname")

      fc = new StarlingBrowserFrameContainer(serverContext, cache, pageBuilder, StarlingHomePage, settings, "Starling " + appName, overriddenUser.name)
      fc.createNewFrame(None, Right(returnHomePageWhenReady _, { case e:UnsupportedOperationException => {}}))

    } })

    val browserService = context.awaitService(classOf[BrowserService])
    val initialData = browserService.initialData
    val userDetails = initialData.userDetails

    Swing.onEDTWait( { settings.setSettings(initialData.settings) })

    context.registerService(classOf[Receiver], new Receiver() {
      def event(event: Event) = {
        onEDT {
          localCachePublisher.publish(event)
          pageContextPublisher.publish(event)
        }
      }
    })

    var bookmarks = initialData.bookmarks
    context.createServiceTracker(Some(classOf[BrowserBundle]), ServiceProperties(), new BromptonServiceCallback[BrowserBundle] {
      def serviceAdded(ref: BromptonServiceReference, properties:ServiceProperties, bundle: BrowserBundle) {
        latch.increment()
        val cache = bundle.initCache()
        onEDT {
          val bundlesPublisher = new Publisher() {}
          bundlesPublisher.listenTo(localCachePublisher)
          bundlesByRef(ref) = (bundle, bundlesPublisher)
          bundlesByName(bundle.bundleName) = bundle
          cacheMap.addAll(cache)
          bundle.addListeners(cacheMap, bundlesPublisher)
          cacheMap(LocalCache.Bookmarks) = toBookmarks(bookmarks)
          pageContextPublisher.publish(BundleAdded(bundle))
          if (bundle == RootBrowserBundle) {
            pageContextPublisher.publish(UserSettingUpdated(UserSettings.ShowPageTime)) //to trigger show page on the first page
          }
          latch.decrement()
          println("Bundle '" + bundle.bundleName + "' started. " + ManagementFactory.getRuntimeMXBean().getUptime()  + "ms")
        }
      }
      override def serviceRemoved(ref: BromptonServiceReference) {
        onEDT {
          val (bundle, bundlesPublisher) = bundlesByRef(ref)
          bundlesPublisher.deafTo(localCachePublisher)
          bundlesByRef.remove(ref)
          bundlesByName.remove(bundle.bundleName)
          //TODO remove values from localcache?
          cacheMap(LocalCache.Bookmarks) = toBookmarks(bookmarks)
          pageContextPublisher.publish(BundleRemoved(bundle))
        }
      }
    })

    context.registerService(classOf[BrowserBundle], RootBrowserBundle)

    def toBookmarks(bookmarks:List[BookmarkLabel]) = {
      bookmarks.map { label => {
        bundlesByName.get(label.bundleName) match {
          case Some(bundle) => BookmarkData(label.owner, label.name, Some(bundle.unmarshal(label.bookmark).asInstanceOf[Bookmark]), label.shared)
          case None => BookmarkData(label.owner, label.name, None, label.shared)
        }
      } }.sortWith{case (l1,l2) => {
        if ((l1.owner == userDetails.username && l2.owner == userDetails.username) || (l1.owner != userDetails.username && l2.owner != userDetails.username)) {
          l1.name.toLowerCase < l2.name.toLowerCase
        } else if (l1.owner == userDetails.username) {
          true
        } else {
          false
        }
      }}
    }

    javax.swing.SwingUtilities.invokeLater(new Runnable {
      def run() {

        cacheMap(LocalCache.Version) = initialData.version
        cacheMap(LocalCache.CurrentUserName) = userDetails
        cacheMap(LocalCache.Bookmarks) = toBookmarks(bookmarks)


        val extraInfo = userDetails.realUsername.map(u => "You are " + userDetails.fullName)
        // Must be called on the EDT
        def sendNotification(notification:Notification) {
          import NotificationKeys._
          cacheMap(AllNotifications) = notification :: cacheMap(AllNotifications)
          cacheMap(UserNotifications) = notification :: cacheMap(UserNotifications)
          fc.updateNotifications()
        }

      localCachePublisher.reactions += {
        case e:Event => {
          bundlesByName.foreach { case (name,bundle) =>
            bundle.notificationHandlers.foreach { handler => {
              handler.handle(e, cache, sendNotification)
            } }
          }
        }
      }
      localCachePublisher.reactions += {
        case e: BookmarksUpdate => {
          bookmarks = e.bookmarks.filter(l => {l.owner == userDetails.username || l.shared})
          cacheMap(LocalCache.Bookmarks) = toBookmarks(bookmarks)
        }
      }
      localCachePublisher.reactions += {
        case GotoPageEvent(p, _) => fc.showNewPage(p)
      }
      latch.decrement()
    } })

    //TODO handle proper restart of browser so that users do not need to restart starling gui on upgrade
    context.onStopped { System.exit(0) }
  }
}