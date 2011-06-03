package starling.pivot.controller

import starling.pivot.TableCell
import starling.quantity.UOM
import starling.pivot.model.AxisCell

import CombinedCell._


case class PivotGrid(
        rowData:Array[Array[AxisCell]],
        colData:Array[Array[AxisCell]],
        mainData:Array[Array[TableCell]],
        colUOMS:Array[UOM] = Array()) {

  lazy val hasData = mainData.exists(_.exists(_.text != ""))

  def combinedData: Array[Array[CombinedCell]] = {
    val rowWidth = rowData.headOption.map(_.size).getOrElse(0)
    val emptyRow = Array.fill(rowWidth)(axisCell(AxisCell.Null))
    val columns = colData.map(col => emptyRow ++ col.map(axisCell))
    val rows = rowData.zip(mainData).map { case (rowData, mainData) => rowData.map(axisCell) ++ mainData.map(tableCell) }

    columns ++ rows
  }
}