package starling.gui.pages

import starling.gui.{StarlingIcons, PageBuildingContext}
import starling.pivot.PivotEdits

case class OrgPage(pivotPageState:PivotPageState) extends AbstractPivotPage(pivotPageState) {
  def text = "Organisation Chart"
  override def icon = StarlingIcons.im("/icons/16x16_organisation.png")
  override def layoutType = Some("Org")
  def selfPage(pivotPageState:PivotPageState, edits:PivotEdits) = {OrgPage(pivotPageState)}
  def dataRequest(pageBuildingContext:PageBuildingContext) = {
    pageBuildingContext.starlingServer.orgPivot(pivotPageState.pivotFieldParams)
  }
}