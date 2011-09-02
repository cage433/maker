package starling.browser.internal

import java.awt.Dimension
import starling.browser.common.MigPanel
import starling.browser._

case class SettingsPage() extends Page {
  def text = "Settings"
  def icon = BrowserIcons.im("/icons/16x16_settings.png")

  def bundle = RootBrowserBundle.bundleName

  def build(serverContext: String) = null
  type SC = String
  def createServerContext(sc: ServerContext) = ""

  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PageData]) = new SettingsPageComponent(context)
}

class SettingsPageComponent(context:PageContext) extends MigPanel("insets 0") with PageComponent {

  val settingsComponents = context.bundles.flatMap(_.settings(context))
  settingsComponents.zipWithIndex.foreach { case (component,index) => {
    add(component, "gapright unrel, ay top")
  }}
}