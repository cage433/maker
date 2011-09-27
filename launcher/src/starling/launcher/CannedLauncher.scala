package starling.launcher

import starling.gui.osgi.GuiBromptonActivator
import starling.browser.osgi.BrowserBromptonActivator
import starling.browser.service.Version._
import starling.singleclasspathmanager.SingleClasspathManager
import starling.manager.{BromptonContext, BromptonActivator}
import starling.browser.service.BrowserService
import starling.browser.{BrowserBundle, ServerContext}
import starling.gui.{CannedBrowserBundle, CannedBrowserService}
import swing.Publisher

/**
 * An alternative StarlingBrowser for testing gui features quickly
 */
object CannedLauncher {
  def main(args:Array[String]) {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    val activators = List(classOf[CannedActivator], classOf[BrowserBromptonActivator])
    val single = new SingleClasspathManager(true, activators)
    single.start()
  }
}

class CannedActivator extends BromptonActivator {
  def start(context: BromptonContext) = {
    context.registerService(classOf[Publisher], new Publisher() {})
    context.registerService(classOf[BrowserService], CannedBrowserService)
    context.registerService(classOf[BrowserBundle], CannedBrowserBundle)
  }
  def stop(context: BromptonContext) = {}
}
