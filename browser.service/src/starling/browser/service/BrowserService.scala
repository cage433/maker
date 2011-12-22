package starling.browser.service

case class UserSettingsEntry(bundle:String, key:String, value:String)
case class UserSettingsLabel(userSettings:List[UserSettingsEntry])
case class BookmarkLabel(owner:String, name:String, bundleName:String, bookmark:String, shared:Boolean)
case class PageLogInfo(text:String,shortText:String,pageString:String, time:java.util.Date)
case class UserLoggedIn(user:String) extends StarlingGUIEvent
case class UserDetails(username:String, fullName:String, realUsername:Option[String]=None)
case class InitialData(name:String, userDetails:UserDetails, settings:UserSettingsLabel, bookmarks:List[BookmarkLabel], version:Version)


trait BrowserService {
  def initialData:InitialData
  def saveSettings(settings:UserSettingsLabel)
  def saveBookmark(bookmark:BookmarkLabel)
  def deleteBookmark(name:String)
  def logPageView(info:PageLogInfo)
  def testEvent()
}