package starling.browser

import swing.Component
import javax.swing.KeyStroke

case class HotKey(keyStroke:KeyStroke, name:String, page:Page)

trait BrowserBundle {

  def bundleName:String
  def marshal(obj:AnyRef):String
  def unmarshal(text:String):AnyRef

  def hotKeys:List[HotKey] = Nil
  def settings(pageContext:PageContext):List[Component] = Nil
  def homeButtons(pageContext:PageContext):List[Component] = Nil
  def userPage(pageContext:PageContext):Option[Page] = None
}