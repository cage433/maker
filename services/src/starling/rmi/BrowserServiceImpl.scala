package starling.rmi

import starling.auth.User
import starling.browser.service._
import starling.manager.Broadcaster
import starling.gui.api.TestEvent

class BrowserServiceImpl(name:String, version:Version, userSettingsDatabase:UserSettingsDatabase, broadcaster:Broadcaster) extends BrowserService {

  def initialData = InitialData(
    name,
    userDetails,
    userSettingsDatabase.loadSettings(User.currentlyLoggedOn),
    userSettingsDatabase.bookmarks(User.currentlyLoggedOn),
    version
  )

  def saveSettings(settings: UserSettingsLabel) = userSettingsDatabase.saveSettings(User.currentlyLoggedOn, settings)

  def saveBookmark(bookmark: BookmarkLabel) = userSettingsDatabase.saveBookmark(User.currentlyLoggedOn, bookmark)

  def deleteBookmark(name: String) = userSettingsDatabase.deleteBookmark(User.currentlyLoggedOn, name)

  def logPageView(info: PageLogInfo) = userSettingsDatabase.logPageView(info)

  def testEvent() = {
    broadcaster.broadcast(TestEvent(User.currentlyLoggedOn.username))
  }

  private def userDetails = {
    val user = User.currentlyLoggedOn
    broadcaster.broadcast(UserLoggedIn(user.username))
    UserDetails(user.username, user.name, user.realUsername)
  }
}