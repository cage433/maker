package starling.pivot

import model.CollapsedState

object HiddenType extends Enumeration {
  type HiddenType = Value
  val NothingHidden, FieldListHidden, AllHidden = Value
}

import HiddenType._

case class OtherLayoutInfo(totals:Totals=Totals.Null, frozen:Boolean=true,
                           rowCollapsedState:CollapsedState=CollapsedState.None,
                           columnCollapsedState:CollapsedState=CollapsedState.None,
                           disabledSubTotals:List[Field]=List(), removeZeros:Boolean=false,
                           hiddenType:HiddenType=NothingHidden, oldHiddenType:Option[HiddenType]=None,
                           oldFrozen:Option[Boolean]=None, columnDetails:ColumnDetails=ColumnDetails.Default)

case class PivotLayout(layoutName:String, pivotFieldState:PivotFieldsState, userLayout:Boolean,
                       otherLayoutInfo:OtherLayoutInfo, layoutType:String, associatedReports:List[String])

object PivotLayout {
  val AssociatedReportsDelimiter = ":::"
}

case class ColumnDetails(expandToFit:Boolean)

object ColumnDetails {
  val Default = ColumnDetails(false)
}