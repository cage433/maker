package starling.browser.osgi

import swing.Publisher
import starling.browser.service.internal.HeterogeneousMap
import starling.browser.service._
import starling.browser.internal.HomePage.StarlingHomePage
import starling.browser.internal._
import javax.swing.UIManager
import com.jgoodies.looks.plastic.PlasticXPLookAndFeel
import starling.manager.{BromptonServiceReference, BromptonServiceTracker, BromptonContext, BromptonActivator}
import swing.Swing._
import starling.browser.common.GuiUtils
import starling.browser._
import swing.event.Event

case class BundleAdded(bundle:BrowserBundle) extends Event
case class BundleRemoved(bundle:BrowserBundle) extends Event

class BrowserPropsFoo
class BrowserBromptonActivator extends BromptonActivator {
  type Props = BrowserPropsFoo

  def defaults = new BrowserPropsFoo

  def init(context: BromptonContext, props: BrowserPropsFoo) { }

  var fc:StarlingBrowserFrameContainer = _

  def start(context: BromptonContext) {

    javax.swing.SwingUtilities.invokeAndWait(new Runnable() { def run() {
      UIManager.getDefaults.put("ClassLoader", classOf[PlasticXPLookAndFeel].getClassLoader)
      GuiUtils.setLookAndFeel()
    } })

    val serverContext = new ServerContext() {
      def username = "username" // TODO - this is not used - can I take it out?
      def lookup[T](klass: Class[T]) = context.awaitService(klass)
      def browserService = context.awaitService(classOf[BrowserService])
    }

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
    val bookmarks = serverContext.browserService.bookmarks
    context.createServiceTracker(Some(classOf[BrowserBundle]), Nil, new BromptonServiceTracker {
      def serviceAdded(ref: BromptonServiceReference, service: AnyRef) {
        val bundle = service.asInstanceOf[BrowserBundle]
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
        BookmarkData(label.name, None) // TODO - we are never returning a bookmark! If I take this out OSGI Gui doesn't load.
      } }
    }
//    val username = serverContext.username

    onEDT({
      val title = browserService.name + " - Starling"

      cacheMap(LocalCache.Version) = serverContext.browserService.version
//      cacheMap(LocalCache.CurrentUserName) = username
      cacheMap(LocalCache.Bookmarks) = toBookmarks(bookmarks)
      cacheMap(NotificationKeys.AllNotifications) = List()
      cacheMap(NotificationKeys.UserNotifications) = List()

      val cache = LocalCache(cacheMap)

      val pageBuilder = new PageBuilder(pageContextPublisher, serverContext, bundlesByName)
      fc = new StarlingBrowserFrameContainer(serverContext, cache, pageBuilder, StarlingHomePage,
        settings, title, serverContext.extraInfo)

      // Must be called on the EDT
      def sendNotification(notification:Notification) {
        import NotificationKeys._
        cacheMap(AllNotifications) = notification :: cacheMap(AllNotifications)
        cacheMap(UserNotifications) = notification :: cacheMap(UserNotifications)
        fc.updateNotifications
      }

      publisher.reactions += {
        case batch:EventBatch => {
          onEDT {
            bundlesByName.foreach { case (name,bundle) =>
              println("Passing  " + batch + " to " + name + " " + bundle.getClass.getClassLoader)
              bundle.notificationHandlers.foreach { handler => {
                batch.events.foreach { e =>
                  handler.handle(e, cache, sendNotification)
                }
              } }
            }
            batch.events.foreach {
//              case e: BookmarksUpdate if e.user == username => {
              case e: BookmarksUpdate if e.user == cacheMap(LocalCache.CurrentUserName) => {
                cacheMap(LocalCache.Bookmarks) = toBookmarks(e.bookmarks)
              }
              case _ =>
            }
            //pageContextPublisher.publish(batch)
          }
        }
      }
    })
  }

  def stop(context: BromptonContext) {
    System.exit(0) //TODO handle proper restart of browser so that users do not need to restart starling gui on upgrade
  }
}