package starling.pivot.model

import starling.pivot.Field
import java.io.Serializable

/**
 * Represents an axis column/row label of 'n' fields
 */
case class AxisValueX(contents : Array[AxisCell])  extends Serializable {
  //def this(l : List[Map[Field,AxisCellValue]]) = this(l.foldLeft(Map.empty[Field,AxisCellValue]){(m, c) => m++c})
  def value(index:Int) = contents(index)
  //def valueOrElse(f : Field, fallback:AxisCellValue):AxisCellValue = contents.getOrElse(f, fallback)
  //def values : Map[Field,Any] = contents
  def size = contents.size
}
