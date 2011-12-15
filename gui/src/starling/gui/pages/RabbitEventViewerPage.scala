package starling.gui.pages

import starling.pivot.PivotEdits
import starling.gui.{LocalCacheKeys, StarlingIcons}
import starling.rabbiteventviewer.api.RabbitEventViewerService
import starling.browser.{ServerContext, LocalCache}
import starling.gui.osgi.StarlingBrowserBundle

class RabbitEventViewerContext(val service:RabbitEventViewerService)

case class RabbitEventViewerPageState(latest:Long)

case class RabbitEventViewerPage(pps:PivotPageState, pageState:RabbitEventViewerPageState) extends AbstractPivotPage(pps) {
  def text = "Rabbit Event Viewer"
  override def icon = StarlingIcons.im("/icons/16x16_event.png")

  def bundle = StarlingBrowserBundle.BundleName
  type SC = RabbitEventViewerContext
  def createServerContext(sc:ServerContext) = new RabbitEventViewerContext(sc.lookup(classOf[RabbitEventViewerService]))

  def selfPage(pivotPageState:PivotPageState, edits:PivotEdits) = copy(pps = pivotPageState)
  def dataRequest(pageBuildingContext:RabbitEventViewerContext) = pageBuildingContext.service.rabbitEvents(pps.pivotFieldParams, pageState.latest)
  override def latestPage(localCache:LocalCache) = {RabbitEventViewerPage(pps, RabbitEventViewerPageState(localCache.localCache(LocalCacheKeys.LatestRabbitEvent)))}
}