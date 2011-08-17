package starling.pivot.view.swing

import org.jfree.data.time.{Day, Month, TimeSeries, TimeSeriesCollection}
import org.jfree.data.general.DatasetUtilities
import org.jfree.chart.{ChartPanel, JFreeChart, ChartFactory}
import javax.swing.{UIManager, SwingUtilities}
import com.jgoodies.looks.plastic.PlasticXPLookAndFeel
import starling.pivot.model._
import scala.swing._
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.chart.axis.{CategoryLabelPositions, CategoryAxis}
import org.jfree.chart.plot.{CategoryPlot, PlotOrientation}
import starling.varcalculator.NAhead
import starling.daterange.{Day => SDay, Month => SMonth}
import starling.pivot.controller.{PivotGrid, PivotTableConverter, PivotTable}
import starling.pivot.{PivotQuantity, Field, TableCell, Totals}
import starling.quantity.Quantity
import starling.browser.common.MigPanel

class Chart
//can't use case classes because of the lazy chart field
class ChartSuccess(_chart: => JFreeChart) extends Chart {
  def chart:JFreeChart = _chart
}
class ChartFailure(_message:String) extends Chart {
  def message = "To enable " + _message
}


/**
 */
class PivotChartView(pivotGrid:PivotGrid) extends MigPanel("insets 0, fill") {
  PivotChartView.createChart("", pivotGrid) match {
    case cf:ChartFailure => { add(new Label(cf.message), "grow")}
    case cs:ChartSuccess => peer.add(new ChartPanel(cs.chart), "grow")
  }
}

object PivotChartView {

  def createChart(title:String, pivotGrid:PivotGrid):Chart = {
    val (rowHeaderCells, columnHeaderCells, mainTableCells) = (pivotGrid.rowData, pivotGrid.colData, pivotGrid.mainData)
    if (columnHeaderCells.size == 0) {
      new ChartFailure("add something to the column header")
    } else if (rowHeaderCells.size == 0) {
      new ChartFailure("add a row field. There must be one row field")
    } else if (rowHeaderCells(0).size > 1) {
      new ChartFailure("remove row fields. There can only be one row field")
    } else {
      if (columnHeaderCells.size == 1 || columnHeaderCells.size == 2) {
        val firstRowHeaderValue = rowHeaderCells(0)(0).value.value.value
        if (firstRowHeaderValue.isInstanceOf[NAhead] || firstRowHeaderValue.isInstanceOf[PivotQuantity] || firstRowHeaderValue.isInstanceOf[Quantity] ) {
          new ChartSuccess(buildXYChart(title, rowHeaderCells, columnHeaderCells, mainTableCells))
        } else if (firstRowHeaderValue.isInstanceOf[starling.daterange.Month]) {
          new ChartSuccess(buildTimeChart(title, rowHeaderCells, columnHeaderCells, mainTableCells))
        } else if (firstRowHeaderValue.isInstanceOf[starling.daterange.Day]) {
          new ChartSuccess(buildTimeChart(title, rowHeaderCells, columnHeaderCells, mainTableCells))
        } else {
          new ChartSuccess(buildBarChart("", rowHeaderCells, columnHeaderCells, mainTableCells))
        }
      } else {
        new ChartFailure("remove a field from the column area")
      }
    }
  }

  private def buildTimeChart(title:String, rowHeaderCells:Array[Array[AxisCell]],
                             columnHeaderCells:Array[Array[AxisCell]], cells:Array[Array[TableCell]]) = {
    val dataset = new TimeSeriesCollection()
    val measureIndex = columnHeaderCells.indexWhere(_(0).isMeasure)
    val labelCells = columnHeaderCells.zipWithIndex.filter { _._2 != measureIndex }.map(_._1)
    val tenor = rowHeaderCells(0)(0).value.value.value match {
      case d:SDay => classOf[Day]
      case m:SMonth => classOf[Month]
    }
    for ((_,columnIndex) <- columnHeaderCells(0).zipWithIndex) {
      val columnName = labelCells.map(_(columnIndex).label).mkString("/")
      val seriesForColumn = new TimeSeries( columnName, tenor)
      for ((row, rowIndex) <- rowHeaderCells.zipWithIndex) {
        val period = row(0).value.value.value match {
          case day:SDay => new Day(day.dayNumber,day.month,day.year)
          case month:SMonth => new Month(month.m, month.y)
        }
        val cell = cells(rowIndex)(columnIndex)
        val value = cell.doubleValue
        val doubleValue:java.lang.Double = value match {
          case Some(d) => d
          case None => null
        }
        seriesForColumn.add( period, doubleValue )
      }
      dataset.addSeries( seriesForColumn )
    }
    ChartFactory.createTimeSeriesChart(
      title,
      rowHeaderCells(0)(0).value.field.name,
      columnHeaderCells(measureIndex)(0).label,
      dataset,
      dataset.getSeriesCount > 1,
      false,
      false
    )
  }

  private def buildXYChart(title:String, rowHeaderCells:Array[Array[AxisCell]],
                             columnHeaderCells:Array[Array[AxisCell]], cells:Array[Array[TableCell]]) = {
    val measureIndex = columnHeaderCells.indexWhere(_(0).isMeasure)
    val labelCells = columnHeaderCells.zipWithIndex.filter { _._2 != measureIndex }.map(_._1)
    val dataset = new XYSeriesCollection()
    for ((column, columnIndex) <- columnHeaderCells(0).zipWithIndex) {
      val columnName = labelCells.map(_(columnIndex).label).mkString("/")
      val seriesForColumn = new XYSeries( columnName )
      for ((row, rowIndex) <- rowHeaderCells.zipWithIndex) {
        val tableCell = cells(rowIndex)(columnIndex)
        val xAsDoubleValue = row(0).value.value.value match {
          case n:NAhead => new java.lang.Double(n.num.doubleValue)
          case q:PivotQuantity => q.doubleValue.getOrElse(null).asInstanceOf[java.lang.Double]
          case q:Quantity => new java.lang.Double(q.value)
        }
        val yAsDoubleValue:java.lang.Double = tableCell.doubleValue.map(new java.lang.Double(_)).getOrElse(null)
        if (xAsDoubleValue != null && yAsDoubleValue != null) {
          seriesForColumn.add( xAsDoubleValue, yAsDoubleValue )
        }
      }
      dataset.addSeries( seriesForColumn )
    }
    ChartFactory.createXYLineChart(
      title,
      rowHeaderCells(0)(0).value.field.name,
      columnHeaderCells(measureIndex)(0).text,
      dataset,
      PlotOrientation.VERTICAL,
      dataset.getSeriesCount > 1,
      false,
      false
    )
  }

  private def buildBarChart(title:String, rowHeaderCells:Array[Array[AxisCell]],
                             columnHeaderCells:Array[Array[AxisCell]], mainTableCells:Array[Array[TableCell]]) = {

    val data = Array.ofDim[Double](columnHeaderCells(0).length, rowHeaderCells.length)
    for ((column, columnIndex) <- columnHeaderCells(0).zipWithIndex) {
      for ((row, rowIndex) <- rowHeaderCells.zipWithIndex) {
        val tableCell = mainTableCells(rowIndex)(columnIndex)
        val dataValue = tableCell.doubleValue
        val doubleValue:Double = dataValue match {
          case Some(d) => d
          case None => 0
        }
        data(columnIndex)(rowIndex) = doubleValue
      }
    }

    val columnHeader:Array[Comparable[_]] = columnHeaderCells(0).map(c=>c.label)
    val rowHeader:Array[Comparable[_]] = rowHeaderCells.map(r=>r(0).label)

    val dataSet = DatasetUtilities.createCategoryDataset(
      columnHeader,
      rowHeader,
      data
    )
    val legend = columnHeader.size > 1 
    val chart = ChartFactory.createBarChart(
      title,
      rowHeaderCells(0)(0).value.field.name,
      if (columnHeaderCells.last.size > 1) "" else columnHeaderCells.last(0).value.value.value.toString,
      dataSet,
      PlotOrientation.VERTICAL, legend, true, false)
    if (rowHeader.size > 5) { //rotate labels if there are too many
      val plot = chart.getPlot().asInstanceOf[CategoryPlot]
      val xAxis = plot.getDomainAxis().asInstanceOf[CategoryAxis]
      xAxis.setCategoryLabelPositions(CategoryLabelPositions.DOWN_45);
    }
    chart    
  }
}