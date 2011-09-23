package starling.browser.osgi

import swing.Publisher
import starling.browser.service.internal.HeterogeneousMap
import starling.browser.service._
import starling.browser.internal.HomePage.StarlingHomePage
import starling.browser.internal._
import javax.swing.UIManager
import com.jgoodies.looks.plastic.PlasticXPLookAndFeel
import starling.manager.{BromptonServiceReference, BromptonServiceCallback, BromptonContext, BromptonActivator}
import swing.Swing._
import starling.browser.common.GuiUtils
import starling.browser._
import swing.event.Event
import java.io.{File, FileInputStream}
import java.util.{Hashtable, Properties}

case class BundleAdded(bundle:BrowserBundle) extends StarlingEvent
case class BundleRemoved(bundle:BrowserBundle) extends StarlingEvent

class BrowserPropsFoo {
  def overriddenUser:String = "NoUser"
}
class BrowserBromptonActivator extends BromptonActivator {
  type Props = BrowserPropsFoo

  def defaults = new BrowserPropsFoo

  def start(context: BromptonContext) {}

  var fc:StarlingBrowserFrameContainer = _

  def init(context: BromptonContext, props: BrowserPropsFoo) {

    javax.swing.SwingUtilities.invokeAndWait(new Runnable() { def run() {
      UIManager.getDefaults.put("ClassLoader", classOf[PlasticXPLookAndFeel].getClassLoader)
      GuiUtils.setLookAndFeel()
    } })

    val serverContext = new ServerContext() {
      def lookup[T](klass: Class[T]) = context.awaitService(klass)
      def browserService = context.awaitService(classOf[BrowserService])
    }

    val userDetails = serverContext.browserService.userDetails

    val localCachePublisher = new Publisher() {}
    val pageContextPublisher = new Publisher() {}
    val browserService = serverContext.browserService
    val settings = new UserSettings(browserService.readSettings, pageContextPublisher)

    val bundlesByRef = new scala.collection.mutable.HashMap[BromptonServiceReference,(BrowserBundle,Publisher)]()
    val bundlesByName = new scala.collection.mutable.HashMap[String,BrowserBundle]()
    val publisher = context.awaitService(classOf[Publisher])
    val cacheMap = new HeterogeneousMap[LocalCacheKey]()

    publisher.reactions += {
      case event => {
        onEDT {
          localCachePublisher.publish(event)
          pageContextPublisher.publish(event)
        }
      }
    }
    var bookmarks = serverContext.browserService.bookmarks
    context.createServiceTracker(Some(classOf[BrowserBundle]), Nil, new BromptonServiceCallback[BrowserBundle] {
      def serviceAdded(ref: BromptonServiceReference, bundle: BrowserBundle) {
        onEDT {
          val bundlesPublisher = new Publisher() {}
          bundlesPublisher.listenTo(localCachePublisher)
          bundlesByRef(ref) = (bundle, bundlesPublisher)
          bundlesByName(bundle.bundleName) = bundle
          bundle.initCache(cacheMap, bundlesPublisher)
          pageContextPublisher.publish(BundleAdded(bundle))
          cacheMap(LocalCache.Bookmarks) = toBookmarks(bookmarks)
        }
      }
      def serviceRemoved(ref: BromptonServiceReference) {
        onEDT {
          val (bundle, bundlesPublisher) = bundlesByRef(ref)
          bundlesPublisher.deafTo(localCachePublisher)
          bundlesByRef.remove(ref)
          bundlesByName.remove(bundle.bundleName)
          //TODO remove values from localcache?
          pageContextPublisher.publish(BundleRemoved(bundle))
          cacheMap(LocalCache.Bookmarks) = toBookmarks(bookmarks)
        }
      }
    })

    context.registerService(classOf[BrowserBundle], RootBrowserBundle)

    /*

      Need to change Browser Launcher so that Browser can respond to bundles being added and removed
      Then GUI might work with osgi (need home page and settings to track bundle events)
      maybe give bundles a handle to initalise the localcache
      Make pages refresh when bundle changes
      Then make bouncy rmi client an osgi bundle
      then get gui to download bundles over http (like booter)
     */

    def toBookmarks(bookmarks:List[BookmarkLabel]) = {
      bookmarks.map { label => {
        bundlesByName.get(label.bundleName) match {
          case Some(bundle) => BookmarkData(label.name, Some(bundle.unmarshal(label.bookmark).asInstanceOf[Bookmark]))
          case None => BookmarkData(label.name, None)
        }
      } }
    }

    javax.swing.SwingUtilities.invokeAndWait(new Runnable {
      def run() {
        val title = browserService.name + " - Starling"

        cacheMap(LocalCache.Version) = serverContext.browserService.version
        cacheMap(LocalCache.CurrentUserName) = userDetails
        cacheMap(LocalCache.Bookmarks) = toBookmarks(bookmarks)
        cacheMap(NotificationKeys.AllNotifications) = List()
        cacheMap(NotificationKeys.UserNotifications) = List()

        val cache = LocalCache(cacheMap)

        val pageBuilder = new PageBuilder(pageContextPublisher, serverContext, bundlesByName)

        val extraInfo = if (props.overriddenUser == "NoUser") None else Some("You are " + props.overriddenUser)
        fc = new StarlingBrowserFrameContainer(serverContext, cache, pageBuilder, StarlingHomePage, settings, title, extraInfo)

        // Must be called on the EDT
        def sendNotification(notification:Notification) {
          import NotificationKeys._
          cacheMap(AllNotifications) = notification :: cacheMap(AllNotifications)
          cacheMap(UserNotifications) = notification :: cacheMap(UserNotifications)
          fc.updateNotifications
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
        case e: BookmarksUpdate if e.user == userDetails.username => {
          bookmarks = e.bookmarks
          cacheMap(LocalCache.Bookmarks) = toBookmarks(bookmarks)
        }
      }
      localCachePublisher.reactions += {
        case GotoPageEvent(p) => fc.showNewPage(p)
      }        
    } })

  }

  def stop(context: BromptonContext) {
    System.exit(0) //TODO handle proper restart of browser so that users do not need to restart starling gui on upgrade
  }
}