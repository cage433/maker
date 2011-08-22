package starling.browser

import common.GuiUtils
import internal._
import internal.HomePage.StarlingHomePage
import service.internal.HeterogeneousMap
import service.{EventBatch, BookmarksUpdate, BookmarkLabel, UserSettingsEntry}
import swing.Publisher
import swing.Swing._
import swing.event.Event

trait NotificationHook {
  def handle(event:Event, cache:LocalCache, addNotification:(Notification=>Unit))
}

object BrowserLauncher {

  def start(notificationHandlers:List[NotificationHook], remotePublisher: Publisher, cacheMap:HeterogeneousMap[LocalCacheKey], extraInfo:Option[String])(serverContext:ServerContext) {

    def toBookmarks(bookmarks:List[BookmarkLabel]) = {
      bookmarks.map { label => {
        val bundle = serverContext.bundleForName(label.bundleName)
        BookmarkData(label.name, bundle.unmarshal(label.bookmark).asInstanceOf[Bookmark])
      } }
    }
    val username = serverContext.username
    val bookmarks = toBookmarks(serverContext.browserService.bookmarks)
    onEDT({
      GuiUtils.setLookAndFeel
      val browserService = serverContext.browserService
      val title = browserService.name + " - Starling"
      val settings = {
        val s = new UserSettings()
        browserService.readSettings.userSettings.foreach {
          case UserSettingsEntry(bundleName, key, value) => {
            s.putSetting(new Key(key, bundleName), serverContext.bundleForName(bundleName).unmarshal(value))
          }
        }
        s
      }

      cacheMap(LocalCache.Version) = serverContext.version
      cacheMap(LocalCache.CurrentUserName) = username
      cacheMap(LocalCache.Bookmarks) = bookmarks
      cacheMap(NotificationKeys.AllNotifications) = List()
      cacheMap(NotificationKeys.UserNotifications) = List()

      val cache = LocalCache(cacheMap)

      val laterPublisher = new Publisher {}
      lazy val fc = new StarlingBrowserFrameContainer(serverContext, cache, laterPublisher, StarlingHomePage,
        settings, title, extraInfo)

      // Must be called on the EDT
      def sendNotification(notification:Notification) {
        import NotificationKeys._
        cacheMap(AllNotifications) = notification :: cacheMap(AllNotifications)
        cacheMap(UserNotifications) = notification :: cacheMap(UserNotifications)
        fc.updateNotifications
      }

      // We want to set up a local cache so that we don't always have to hit the server. This cache is populated on startup and then listens to
      // the publisher to stay fresh.
      remotePublisher.reactions += {
        case batch:EventBatch => {
          notificationHandlers.foreach { handler => {
            batch.events.foreach { e =>
              handler.handle(e, cache, sendNotification)
            }
          } }
          batch.events.foreach {
            case e: BookmarksUpdate if e.user == username => {
              cacheMap(LocalCache.Bookmarks) = toBookmarks(e.bookmarks)
            }
            case _ =>
          }
          laterPublisher.publish(batch)
        }
      }

      fc
    })
  }

  var numberOfClientsLaunched = 0

}