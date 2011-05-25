package starling.gui.pages

import starling.gui.{StarlingIcons, PageBuildingContext}

case class UserStatsPage(pivotPageState:PivotPageState) extends AbstractPivotPage(pivotPageState) {
  val text = "User Stats"
  override val icon = StarlingIcons.im("/icons/16x16_stats.png")
  override def layoutType = Some("UserStats")

  def selfPage(pps:PivotPageState) = copy(pivotPageState = pps)
  def dataRequest(pageBuildingContext:PageBuildingContext) = {
    pageBuildingContext.starlingServer.userStatsPivot(pivotPageState.pivotFieldParams)
  }
}