package starling.browser.service

import java.awt.Color

case class Version(name:String, hostname:String, database:String, gitCommit:String, production:Boolean, colour:Option[Color])

case class BookmarksUpdate(bookmarks:List[BookmarkLabel]) extends StarlingGUIEvent

