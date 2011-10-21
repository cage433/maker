package starling.browser.service

import starling.manager.DoNotCache

case class UserSettingsEntry(bundle:String, key:String, value:String)
case class UserSettingsLabel(userSettings:List[UserSettingsEntry])
case class BookmarkLabel(name:String, bundleName:String, bookmark:String)
case class PageLogInfo(text:String,shortText:String,pageString:String, time:java.util.Date)
case class UserLoggedIn(user:String) extends StarlingGUIEvent
case class UserDetails(username:String, fullName:String, realUsername:Option[String]=None)

trait BrowserService {
  def name:String
  @DoNotCache def userDetails:UserDetails
  @DoNotCache def readSettings:UserSettingsLabel
  @DoNotCache def saveSettings(settings:UserSettingsLabel)
  @DoNotCache def saveBookmark(bookmark:BookmarkLabel)
  @DoNotCache def deleteBookmark(name:String)
  @DoNotCache def bookmarks:List[BookmarkLabel]
  @DoNotCache def logPageView(info:PageLogInfo)
  @DoNotCache def testEvent()
  def version:Version
}