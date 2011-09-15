package starling.rmi

import starling.auth.User
import starling.browser.service._
import starling.utils.Broadcaster
import starling.gui.api.TestEvent

class BrowserServiceImpl(val name:String, val version:Version, userSettingsDatabase:UserSettingsDatabase, broadcaster:Broadcaster) extends BrowserService {
  def readSettings = userSettingsDatabase.loadSettings(User.currentlyLoggedOn)

  def saveSettings(settings: UserSettingsLabel) = userSettingsDatabase.saveSettings(User.currentlyLoggedOn, settings)

  def saveBookmark(bookmark: BookmarkLabel) = userSettingsDatabase.saveBookmark(User.currentlyLoggedOn, bookmark)

  def deleteBookmark(name: String) = userSettingsDatabase.deleteBookmark(User.currentlyLoggedOn, name)

  def bookmarks = userSettingsDatabase.bookmarks(User.currentlyLoggedOn)

  def logPageView(info: PageLogInfo) = userSettingsDatabase.logPageView(info)

  def testEvent() = {
    broadcaster.broadcast(TestEvent(User.currentlyLoggedOn.username))
  }

  def userDetails = {
    val user = User.currentlyLoggedOn
    broadcaster.broadcast(UserLoggedIn(user.username))
    UserDetails(user.username, user.name)
  }
}