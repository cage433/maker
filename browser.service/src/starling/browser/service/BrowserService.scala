package starling.browser.service

import scala.swing.event.Event

case class UserSettingsEntry(bundle:String, key:String, value:String)
case class UserSettingsLabel(userSettings:List[UserSettingsEntry])
case class BookmarkLabel(name:String, bundleName:String, bookmark:String)
case class PageLogInfo(text:String,shortText:String,pageString:String, time:java.util.Date)
case class UserLoggedIn(user:String) extends Event
case class UserDetails(username:String, fullName:String)

trait BrowserService {
  def name:String
  def userDetails:UserDetails
  def readSettings:UserSettingsLabel
  def saveSettings(settings:UserSettingsLabel)
  def saveBookmark(bookmark:BookmarkLabel)
  def deleteBookmark(name:String)
  def bookmarks:List[BookmarkLabel]
  def logPageView(info:PageLogInfo)
  def testEvent()
  def version:Version
}