package starling.pivot

import java.io.Serializable
import model.{UndefinedValueNew, NoValue, UndefinedValue}
import starling.quantity.{Percentage, Quantity}
import starling.utils.Pattern._
import starling.utils.ImplicitConversions._
import starling.utils.StarlingObject


object EditableCellState extends Enumeration {
  type EditableCellState = Value
  val Normal, Tainted, Edited, Deleted, Added, AddedBlank, Error, Unvalidated= Value
}
import EditableCellState._

/**
 * Represents a cell in the Pivot table. Contains a value and rendering information
 */
case class TableCell(value:Any, text:String, textPosition:TextPosition = CenterTextPosition, isError:Boolean = false,
                     totalState:TotalState = NotTotal, editable:Boolean = false, state:EditableCellState = Normal,
                     longText:Option[String] = None, warning:Option[String]= None, edits:PivotEdits=PivotEdits.Null,
                     originalValue:Option[Any]=None) {
  def this(value:Any) = this(value, value.toString)

  def asString = text

  def doubleValue:Option[Double] = {
    value match {
      case pq:PivotQuantity => pq.doubleValue
      case d:Double => Some(d)
      case l:Long => Some(l.doubleValue)
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
      case l:Long => Some(l.doubleValue)
      case i:Int => Some(i.doubleValue)
      case p:Percentage => Some(p.decimalValue)
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

  val Null = new TableCell(NoValue, "")
  val EditableNull = new TableCell(NoValue, "", editable = true)
  val Undefined = new TableCell(UndefinedValue, "n/a")
  val UndefinedNew = new TableCell(UndefinedValueNew, UndefinedValueNew.toString)
  val BlankAddedCell = new TableCell(UndefinedValueNew, "", RightTextPosition, editable = true, state = AddedBlank)

  def longText(pq: PivotQuantity) =
    (pq.explanation, pq.warning) match {
      case (None, None) => None
      case (Some(e), None) => Some(e)
      case (None, Some(w)) => Some("Validation Error: " + w)
      case (Some(e), Some(w)) => Some(e + " Validation Error: " + w)
    }

  def fromPivotQuantity(pq: PivotQuantity, formatInfo: ExtraFormatInfo): TableCell = {
    val (shortText, longText) = PivotFormatter.shortAndLongText(pq, formatInfo)
    new TableCell(pq, shortText, if (pq.isOnlyErrors) RightTextPosition else RightTextPosition, pq.hasErrors, longText = longText, warning = pq.warning)
  }

  val PivotQuantity: Extractor[TableCell, PivotQuantity] = Extractor.from[TableCell](_.pivotQuantityValue)
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
