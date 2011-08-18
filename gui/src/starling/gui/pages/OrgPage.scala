package starling.gui.pages

import starling.gui.{StarlingIcons, StarlingServerContext}
import starling.pivot.PivotEdits

case class OrgPage(pivotPageState:PivotPageState) extends AbstractPivotPage(pivotPageState) {
  def text = "Organisation Chart"
  override def icon = StarlingIcons.im("/icons/16x16_organisation.png")
  def selfPage(pivotPageState:PivotPageState, edits:PivotEdits) = {OrgPage(pivotPageState)}
  def dataRequest(pageBuildingContext:StarlingServerContext) = {
    pageBuildingContext.server.orgPivot(pivotPageState.pivotFieldParams)
  }
}