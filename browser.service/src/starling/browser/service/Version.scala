package starling.browser.service

import java.awt.Color
import scala.swing.event.Event

case class Version(name:String, hostname:String, database:String, production:Boolean, colour:Option[Color])

case class BookmarksUpdate(user:String, bookmarks:List[BookmarkLabel]) extends Event

