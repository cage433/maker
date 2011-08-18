package starling.browser

import swing.Component

trait BrowserBundle {

  def bundleName:String

  def marshal(obj:AnyRef):String
  def unmarshal(text:String):AnyRef

  def settings(pageContext:PageContext):List[Component]

  def homeButtons(pageContext:PageContext):List[Component]
}