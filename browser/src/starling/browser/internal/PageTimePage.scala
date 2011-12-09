package starling.browser.internal

import starling.browser._
import common.MigPanel
import java.awt.Dimension
import starling.manager.TimeTree
import swing.TextArea

case class PageTimePage(timeTree:TimeTree) extends Page {
  def bundle = RootBrowserBundle.bundleName
  def icon = BrowserIcons.im("/icons/weather-clear.png")
  def text = "Page Time"
  def createComponent(context: PageContext, data: PageData, bookmark: Bookmark, browserSize: Dimension, previousPageData: Option[PreviousPageData]) = {
    new PageTimeComponent(timeTree)
  }

  def build(serverContext: String) = new PageData {}
  type SC = String
  def createServerContext(sc: ServerContext) = ""
}



class PageTimeComponent(timeTree:TimeTree) extends MigPanel("") with PageComponent {
  val textArea = new TextArea(timeTree.text())
  add(textArea, "")
}