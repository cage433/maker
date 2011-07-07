package starling.pivot.controller

import starling.pivot.TableCell
import starling.pivot.model.AxisCell

import starling.utils.Pattern._


case class CombinedCell(value: Either[AxisCell, TableCell]) {
  val (axisCell, tableCell) = (value.left.toOption, value.right.toOption)

  override def toString = value.fold(_.toString, _.toString)
}

object CombinedCell {
  def axisCell(value: AxisCell)   = CombinedCell(Left(value))
  def tableCell(value: TableCell) = CombinedCell(Right(value))
  val AxisCell  = Extractor.from[CombinedCell](_.axisCell)
  val TableCell = Extractor.from[CombinedCell](_.tableCell)
}