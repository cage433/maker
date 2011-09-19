package starling.gui.pages

import starling.pivot.PivotEdits
import starling.browser.LocalCache
import starling.gui.{LocalCacheKeys, StarlingServerContext, StarlingIcons}

case class RabbitEventViewerPageState(latest:Long)

case class RabbitEventViewerPage(pps:PivotPageState, pageState:RabbitEventViewerPageState) extends AbstractStarlingPivotPage(pps) {
  def text = "Rabbit Event Viewer"
  override def icon = StarlingIcons.im("/icons/16x16_event.png")
  def selfPage(pivotPageState:PivotPageState, edits:PivotEdits) = copy(pps = pivotPageState)
  def dataRequest(pageBuildingContext:StarlingServerContext) = pageBuildingContext.server.rabbitEvents(pps.pivotFieldParams, pageState.latest)
  override def latestPage(localCache:LocalCache) = {RabbitEventViewerPage(pps, RabbitEventViewerPageState(localCache.localCache(LocalCacheKeys.LatestRabbitEvent)))}
}