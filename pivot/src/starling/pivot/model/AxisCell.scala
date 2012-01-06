package starling.pivot.model

import starling.pivot._
import starling.pivot.EditableCellState._
import starling.utils.Pattern.Extractor

case class AxisCell(value:AxisValue, span:Option[Int], label:String, longLabel:String, collapsible:Option[Boolean],  hidden:Boolean,
                    totalState:TotalState, offset:Int, textPosition:TextPosition, editable:Boolean = false,
                    overrideState:Option[CellState]=None, overrideValue:Option[Any]=None, changed:Boolean=false, duplicateText:Option[String]=None) {
  def text = if (!hidden) label else ""
  def textForCopy = label
  def state = overrideState.getOrElse(value.state)
  def actualValue = overrideValue.getOrElse(value.value.value)
  def selection = value.field → SomeSelection(Set(value.value.originalValue.getOrElse(value.value.value)))
  def edits = value.pivotEdits
  def shown = !hidden
  def changeLabel(label:String) = copy(label=label)
  def hide = copy(hidden=true)
  def isTotalValue = value.value == TotalAxisValueType
  def isMeasure= value.value.isInstanceOf[MeasureAxisValueType]
  def notTotalValue = !isTotalValue
  override def toString = text
  def valueText = value.valueText
}
object AxisCell {
  val Null = AxisCell(AxisValue(Field.NullField, NullAxisValueType, 0), Some(1), "", "", None, true, NotTotal, 0, LeftTextPosition)
  val NullTotal = AxisCell(AxisValue(Field.NullField, TotalAxisValueType, 0), Some(1), "", "", None, true, Total, 0, LeftTextPosition)
  val Filler = AxisCell(AxisValue(Field.NullField, NullAxisValueType, 0), Some(1), "", "", None, true, NotTotal, 0, LeftTextPosition)
  val ValueText: Extractor[AxisCell, String] = Extractor.from[AxisCell](cell => Some(cell.valueText))
  val Value: Extractor[AxisCell, Any] = Extractor.from[AxisCell](cell => Some(cell.value.value.value))
  val BlankAddedCell = AxisCell(AxisValue(Field.NullField, BlankAddedAxisValueType, 0), Some(1), "", "", None, false, NotTotal, 0, LeftTextPosition, editable = true)
}


