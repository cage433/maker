package starling.pivot.model

import starling.pivot.Field
import java.io.Serializable

/**
 * Represents a horizontal or vertical axis for the pivot table
 */
case class AxisTotals(grandTotal:Boolean, subTotals:Boolean)
object AxisTotals {
  val Null = AxisTotals(false, false)
}
