package starling.pivot.model

import collection.mutable.ListBuffer
import starling.pivot.{Field, PivotResult}

/**
 * Used by the table building code to define the fields
 * and columns required per axis
 *
 * e.g. field values + subtotal
 */
class DataAxis(fields: List[Field], pivotResult: PivotResult, intermediates: Boolean, finals: Boolean) {
  def this(f: List[Field], pivotResult: PivotResult, intermediates : Boolean) = this (f, pivotResult, intermediates, true)
  def this(f: List[Field], pivotResult: PivotResult) = this (f, pivotResult, false, true)


  /**
   * returns a label consisting of the fields concatenated together
   */
  def getLabel = fields.map {f => f.name}.mkString("/")

  /**
   *  returns the fields on the axis
   */
  def getFields = fields


  /** returns whether there are final values*/
  def getFinals = finals
}

