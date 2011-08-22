package starling.browser

import swing.Component
import javax.swing.KeyStroke
import java.awt.image.BufferedImage

case class HotKey(keyStroke:KeyStroke, name:String, page:Page)
trait PageFactory {
  def create(serverContext:ServerContext):Page
}
class PagePageFactory(page:Page) extends PageFactory {
  def create(serverContext: ServerContext) = page
}
class PageButton(val name:String, val pageFactory:PageFactory, val icon:BufferedImage, val key:Option[KeyStroke] = None)

object PageButton {
  def apply(name:String, page:Page, icon:BufferedImage) = {
    new PageButton(name, new PagePageFactory(page), icon)
  }
}
trait BrowserBundle {

  def bundleName:String
  def marshal(obj:AnyRef):String
  def unmarshal(text:String):AnyRef

  def hotKeys:List[HotKey] = Nil
  def settings(pageContext:PageContext):List[Component] = Nil
  def homeButtons(pageContext:PageContext):List[PageButton] = Nil
  def userPage(pageContext:PageContext):Option[Page] = None
}