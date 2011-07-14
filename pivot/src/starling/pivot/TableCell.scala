package starling.pivot

import java.io.Serializable
import model.UndefinedValue

import starling.quantity.{Percentage, Quantity}
import starling.utils.Pattern._
import starling.utils.ImplicitConversions._
import starling.utils.StarlingObject


object EditableCellState extends Enumeration {
  type EditableCellState = Value
  val Normal, Edited, Error, Added, Deleted, Tainted = Value
}
import EditableCellState._

/**
 * Represents a cell in the Pivot table. Contains a value and rendering information
 */
case class TableCell(value:Any, text:String, textPosition:TextPosition = CenterTextPosition, isError:Boolean = false,
                     totalState:TotalState = NotTotal, editable:Boolean = false, state:EditableCellState = Normal, longText:Option[String] = None,
                     warning:Option[String]= None, edits:Set[PivotEdit]=Set.empty) {
  def this(value:Any) = this(value, value.toString)

  def asString = text
//  override def toString = text
  def doubleValue:Option[Double] = {
    value match {
      case pq:PivotQuantity => pq.doubleValue
      case d:Double => Some(d)
      case i:Int => Some(i.doubleValue)
      case p:Percentage => Some(p.decimalValue)
      case _ => None
    }
  }
  def doubleValueIgnoringErrors:Option[Double] = {
    value match {
      case pq:PivotQuantity => pq.doubleValueIgnoringErrors
      case q:Quantity => Some(q.value)
      case d:Double => Some(d)
      case i:Int => Some(i.doubleValue)
      case _ => None
    }
  }
  def pivotQuantityValue: Option[PivotQuantity] = value.safeCast[PivotQuantity]
  def errors:Set[StackTrace] = {
    if (!isError) {
      Set()
    } else {
      value match {
        case pq:PivotQuantity => pq.errors.values.flatten.toSet
        case _ => Set()
      }
    }
  }
}

object TableCell {
  def apply(value: Any) = new TableCell(value)

  val Null = new TableCell(Set(), "")
  val Undefined = new TableCell(UndefinedValue)

  def longText(pq: PivotQuantity) =
    (pq.explanation, pq.warning) match {
      case (None, None) => None
      case (Some(e), None) => Some(e)
      case (None, Some(w)) => Some("Validation Error: " + w)
      case (Some(e), Some(w)) => Some(e + " Validation Error: " + w)
    }

  def fromPivotQuantity(pq: PivotQuantity, formatInfo: ExtraFormatInfo): TableCell = {
    val t = longText(pq)
    new TableCell(pq, PivotFormatter.formatPivotQuantity(pq, formatInfo), RightTextPosition, pq.hasErrors, longText = t, warning = pq.warning)
  }

  val PivotQuantity = Extractor.from[TableCell](_.pivotQuantityValue)
}

class TotalState extends Serializable
object NotTotal extends TotalState with StarlingObject
object Total extends TotalState with StarlingObject
object SubTotal extends TotalState with StarlingObject
object SubtotalTotal extends TotalState with StarlingObject
object OtherValueTotal extends TotalState with StarlingObject

class TextPosition extends Serializable
object CenterTextPosition extends TextPosition with StarlingObject
object RightTextPosition extends TextPosition with StarlingObject
object LeftTextPosition extends TextPosition with StarlingObject
