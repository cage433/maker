package starling.rmi

import starling.browser.service.{UserSettingsLabel, BookmarkLabel, PageLogInfo, BrowserService}
import starling.auth.User

class BrowserServiceImpl(val name:String, userSettingsDatabase:UserSettingsDatabase) extends BrowserService {
  def readSettings = userSettingsDatabase.loadSettings(User.currentlyLoggedOn)

  def saveSettings(settings: UserSettingsLabel) = userSettingsDatabase.saveSettings(User.currentlyLoggedOn, settings)

  def saveBookmark(bookmark: BookmarkLabel) = userSettingsDatabase.saveBookmark(User.currentlyLoggedOn, bookmark)

  def deleteBookmark(name: String) = userSettingsDatabase.deleteBookmark(User.currentlyLoggedOn, name)

  def bookmarks = userSettingsDatabase.bookmarks(User.currentlyLoggedOn)

  def logPageView(info: PageLogInfo) = userSettingsDatabase.logPageView(info)
}