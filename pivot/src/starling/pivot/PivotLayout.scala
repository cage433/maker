package starling.pivot

import model.CollapsedState

case class OtherLayoutInfo(totals:Totals=Totals.Null, frozen:Boolean=true, fieldPanelCollapsed:Boolean=false,
                           rowCollapsedState:CollapsedState=CollapsedState.None,
                           columnCollapsedState:CollapsedState=CollapsedState.None,
                           disabledSubTotals:List[Field]=List(), removeZeros:Boolean=false)

case class PivotLayout(layoutName:String, pivotFieldState:PivotFieldsState, userLayout:Boolean,
                       otherLayoutInfo:OtherLayoutInfo, layoutType:String, associatedReports:List[String])

object PivotLayout {
  val AllLayoutType = "All"
  val ReportLayoutType = "Report"
  val BlankLayout = PivotLayout("Blank", PivotFieldsState(), false, OtherLayoutInfo(Totals.Null), AllLayoutType, List())
  val AssociatedReportsDelimiter = ":::"
}