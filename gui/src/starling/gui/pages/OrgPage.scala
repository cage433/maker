package starling.gui.pages

import starling.pivot.{PivotFieldsState, PivotFieldParams}
import starling.gui.{StarlingIcons, PageBuildingContext}

case class OrgPage(pivotPageState:PivotPageState) extends AbstractPivotPage(pivotPageState) {
  def text = "Organisation Chart"
  override def icon = StarlingIcons.im("/icons/16x16_organisation.png")
  override def layoutType = Some("Org")
  def selfPage(pivotPageState:PivotPageState) = {OrgPage(pivotPageState)}
  def dataRequest(pageBuildingContext:PageBuildingContext) = {
    pageBuildingContext.starlingServer.orgPivot(pivotPageState.pivotFieldParams)
  }
}