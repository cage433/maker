package starling.gui.pages

import starling.pivot.PivotEdits
import starling.gui.{StarlingServerContext, StarlingIcons}

case class GitLogPage(pps:PivotPageState) extends AbstractStarlingPivotPage(pps) {
  def text = "Git Log"
  override def icon = StarlingIcons.im("/icons/16x16_log.png")
  def selfPage(pivotPageState:PivotPageState, edits:PivotEdits) = copy(pps = pivotPageState)
  def dataRequest(pageBuildingContext:StarlingServerContext) = pageBuildingContext.server.gitLog(pps.pivotFieldParams, 1000)
}