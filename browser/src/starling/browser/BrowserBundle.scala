package starling.browser

import internal.Notification
import service.internal.HeterogeneousMap
import java.awt.image.BufferedImage
import javax.swing.{ImageIcon, KeyStroke}
import swing.{Publisher, Component}
import swing.event.Event

case class HotKey(keyStroke:KeyStroke, name:String, page:Page)
case class HelpEntry(markup:String, icons:Map[String,ImageIcon], links:Map[String,Page])

trait PageFactory {
  def create(serverContext:ServerContext):Page
}
class PagePageFactory(page:Page) extends PageFactory {
  def create(serverContext: ServerContext) = page
}
class PageButton(val name:String, val pageFactory:PageFactory, val icon:BufferedImage, val key:Option[KeyStroke] = None) {
  val tooltip:Option[String] = key.map(k => {name + " (" + String.valueOf(k.getKeyCode.asInstanceOf[Char]).toUpperCase + ")"})
}

object PageButton {
  def apply(name:String, page:Page, icon:BufferedImage, key:Option[KeyStroke]) = {
    new PageButton(name, new PagePageFactory(page), icon, key)
  }
}

trait NotificationHook {
  def handle(event:Event, cache:LocalCache, addNotification:(Notification=>Unit))
}

trait BrowserBundle {

  def bundleName:String
  def marshal(obj:AnyRef):String
  def unmarshal(text:String):AnyRef

  def initCache(cache:HeterogeneousMap[LocalCacheKey], publisher:Publisher)

  def hotKeys:List[HotKey] = Nil
  def settings(pageContext:PageContext):List[Component] = Nil
  def homeButtons(pageContext:PageContext):List[PageButton] = Nil
  def userPage(pageContext:PageContext):Option[Page] = None
  def helpEntries:List[HelpEntry] = Nil
  def notificationHandlers:List[NotificationHook] = Nil
}

case class GotoPageEvent(page:Page) extends Event