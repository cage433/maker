package starling.pivot.model

import starling.pivot._
import starling.pivot.EditableCellState._

case class AxisCell(value:AxisValue, span:Option[Int], label:String, collapsible:Option[Boolean],  hidden:Boolean,
                    totalState:TotalState, offset:Int, textPosition:TextPosition, editable:Boolean = false,
                    state:EditableCellState = Normal) {
  def text = if (!hidden) label else ""
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
  val Null = AxisCell(AxisValue(Field.NullField, NullAxisValueType, 0), Some(1), "", None, true, NotTotal, 0, LeftTextPosition)
  val Filler = AxisCell(AxisValue(Field.NullField, NullAxisValueType, 0), Some(1), "", None, true, NotTotal, 0, LeftTextPosition)
}


