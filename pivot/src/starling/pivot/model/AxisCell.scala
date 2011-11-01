package starling.pivot.model

import starling.pivot._
import starling.pivot.EditableCellState._
import starling.utils.Pattern.Extractor

case class AxisCell(value:AxisValue, span:Option[Int], label:String, longLabel:String, collapsible:Option[Boolean],  hidden:Boolean,
                    totalState:TotalState, offset:Int, textPosition:TextPosition, editable:Boolean = false,
                    overrideState:Option[EditableCellState]=None) {
  def text = if (!hidden) label else ""
  def state = {
    overrideState match {
      case Some(s) => s
      case _ => value.state
    }
  }
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

  @transient var changed = false
}
object AxisCell {
  val Null = AxisCell(AxisValue(Field.NullField, NullAxisValueType, 0), Some(1), "", "", None, true, NotTotal, 0, LeftTextPosition)
  val NullTotal = AxisCell(AxisValue(Field.NullField, TotalAxisValueType, 0), Some(1), "", "", None, true, Total, 0, LeftTextPosition)
  val Filler = AxisCell(AxisValue(Field.NullField, NullAxisValueType, 0), Some(1), "", "", None, true, NotTotal, 0, LeftTextPosition)
  val ValueText: Extractor[AxisCell, String] = Extractor.from[AxisCell](cell => Some(cell.valueText))
  val Value: Extractor[AxisCell, Any] = Extractor.from[AxisCell](cell => Some(cell.value.value.value))
}


