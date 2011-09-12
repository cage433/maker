package starling.gui

import javax.swing.{UIManager, SwingUtilities}
import com.jgoodies.looks.plastic.PlasticXPLookAndFeel
import starling.pivot.model._
import scala.swing._
import org.testng.Assert._
import org.jfree.chart.plot.{XYPlot, CategoryPlot}
import starling.pivot.view.swing.{ChartSuccess, ChartFailure, PivotChartView}
import starling.varcalculator.NAhead
import org.scalatest.testng.TestNGSuite
import starling.pivot.controller.PivotGrid
import starling.daterange.{Month, Day}
import starling.pivot.{LeftTextPosition, NotTotal, Field, TableCell}
import starling.browser.common.MigPanel

object PivotChartViewTest {

  private def ac(field:String, value:Any) = {
    AxisCell(
      AxisValue(Field(field), ValueAxisValueType(value), 0),
      Some(1), value.toString, None, false, NotTotal, 0, LeftTextPosition
    )
  }

  private def ac(field:String) = {
    AxisCell(
      AxisValue(Field("F"), MeasureAxisValueType(Field(field)), 0),
      Some(1), field, None, false, NotTotal, 0, LeftTextPosition
    )
  }

  val pivotTableDeltaByMonth = {
    PivotGrid(
     Array(
       Array(ac("Month", Month(2010, 1))),
       Array(ac("Month", Month(2010, 2))),
       Array(ac("Month", Month(2010, 3)))
     ),
     Array(
       Array(ac("Position"))
     ),
     Array(
        Array( TableCell(20) ),
        Array( TableCell(25) ),
        Array( TableCell(18) )
     ))
  }

  val pivotTableDeltaByMonthAndMarket = {
    PivotGrid(
     Array(
       Array(ac("Month", Month(2010, 1))),
       Array(ac("Month", Month(2010, 2))),
       Array(ac("Month", Month(2010, 3)))
     ),
     Array(
       Array(ac("Market", "Lead"), ac("Market", "Copper"), ac("Market", "Steel")),
       Array(ac("Position"), ac("Position"), ac("Position"))
     ),
     Array(
        Array( TableCell(20), TableCell(30), TableCell(33) ),
        Array( TableCell(20), TableCell(30), TableCell(32) ),
        Array( TableCell(20), TableCell(30), TableCell(31) )
     ))
  }

  val pivotTableDeltaWithDMinus1ByMonth = {
    PivotGrid(
     Array(
       Array(ac("Month", Month(2010, 1))),
       Array(ac("Month", Month(2010, 2))),
       Array(ac("Month", Month(2010, 3)))
     ),
     Array(
       Array(ac("Position")),
       Array(ac("Position D-1"))
     ),
     Array(
        Array( TableCell(20), TableCell(23) ),
        Array( TableCell(20), TableCell(18) ),
        Array( TableCell(30), TableCell(31) )
     ))
  }

  val pivotTableSpotFXPriceHistory = {
    PivotGrid(
     Array(
       Array(ac("Observation Day", Day(2010, 1,  1))),
       Array(ac("Observation Day", Day(2010, 2, 12))),
       Array(ac("Observation Day", Day(2010, 4,  3)))
     ),
     Array(
       Array(ac("Market", "GBP"), ac("Market", "EUR"), ac("Market", "USD")),
       Array(ac("Price"), ac("Price"), ac("Price"))
     ),
     Array(
        Array( TableCell(20), TableCell(23), TableCell(17) ),
        Array( TableCell(20), TableCell(18), TableCell(18) ),
        Array( TableCell(30), TableCell(31), TableCell(20) )
     ))
  }

  val pivotTableNDaysForwardCurve = {
    PivotGrid(
      Array(
       Array(ac("Ndays ahead", NAhead(1, "Day"))),
       Array(ac("Ndays ahead", NAhead(3, "Day"))),
       Array(ac("Ndays ahead", NAhead(30, "Day")))
      ),
      Array(
       Array(ac("Market", "Lead"), ac("Market", "Copper")),
       Array(ac("Price"), ac("Price"))
      ),
      Array(
        Array( TableCell(400), TableCell(280) ),
        Array( TableCell(420), TableCell(270) ),
        Array( TableCell(430), TableCell(250) )
      )
    )
  }
}
class PivotChartViewTest extends TestNGSuite {

//  @Test
//  def testOneRowFieldAndOneDataField() {
//    val chart = PivotChartView.createChart("", pivotTableDeltaByMonth)
//    chart match {
//      case c:ChartSuccess => {
//        val plot = chart.getPlot().asInstanceOf[CategoryPlot]
//        val xAxis = plot.getDomainAxis().asInstanceOf[CategoryAxis]
//        assertEquals(xAxis.
//      }
//      case f:ChartFailure => fail("One row and one field should create a chart")
//    }
//  }

  //@Test
  def testDaySeriesWithDayAsRowField() {
    val chart = PivotChartView.createChart("", PivotChartViewTest.pivotTableSpotFXPriceHistory)
    chart match {
      case c:ChartSuccess => {
        val dataSet = c.chart.getPlot().asInstanceOf[XYPlot].getDataset
        assertEquals(dataSet.getSeriesCount, 2) //2 currencies
        assertEquals(dataSet.getSeriesCount, 2) //2 currencies
        assertEquals(dataSet.getItemCount(0), 3) //3 days
        assertEquals(dataSet.getSeriesKey(0), "EUR")
        assertEquals(c.chart.getPlot().asInstanceOf[XYPlot].getDomainAxis.getLabel, "Observation Day")
        assertEquals(c.chart.getPlot().asInstanceOf[XYPlot].getRangeAxis.getLabel, "Price")
      }
      case f:ChartFailure => fail("One row and one data field should create a chart")
    }
  }

  //@Test
  def testBarChartWithColumnField() {
    val chart = PivotChartView.createChart("", PivotChartViewTest.pivotTableDeltaByMonthAndMarket)
    chart match {
      case c:ChartSuccess => {
        val dataSet = c.chart.getPlot().asInstanceOf[CategoryPlot].getDataset
        assertEquals(dataSet.getRowCount, 2) //2 markets
        assertEquals(dataSet.getColumnCount, 3) //3 months
        assertEquals(dataSet.getValue(1, 0), 20.0)//test one value
        assertEquals(c.chart.getPlot().asInstanceOf[CategoryPlot].getDomainAxis.getLabel, "Month")
        assertEquals(c.chart.getPlot().asInstanceOf[CategoryPlot].getRangeAxis.getLabel, "Position")
      }
      case f:ChartFailure => fail("One row, column and data field should create a chart")
    }
  }

  //@Test
  def testBarChartWithManyDataFields() {
    val chart = PivotChartView.createChart("", PivotChartViewTest.pivotTableDeltaWithDMinus1ByMonth)
    chart match {
      case c:ChartSuccess => {
        val dataSet = c.chart.getPlot().asInstanceOf[CategoryPlot].getDataset
        assertEquals(dataSet.getRowCount, 2) //2 data fields
        assertEquals(dataSet.getColumnCount, 3) //3 months
        assertEquals(dataSet.getValue(1, 0), 25.0)//test one value
        assertEquals(c.chart.getPlot().asInstanceOf[CategoryPlot].getDomainAxis.getLabel, "Month")
        assertEquals(c.chart.getPlot().asInstanceOf[CategoryPlot].getRangeAxis.getLabel, "") //no Y axis label because there are two data fields
      }
      case f:ChartFailure => fail("One row and no column and many data fields should create a chart")
    }
  }

}

class PivotChartFrame(pivotGrids:List[PivotGrid]) extends MainFrame {
  title = "Chart tester"
  contents = new ScrollPane(new MigPanel("") {
    pivotGrids.foreach {
      pivotGrid => add(new PivotChartView(pivotGrid), "wrap")
    }
  })
}

object PivotChartViewMain {
  def main(args:Array[String]) {
    SwingUtilities.invokeLater(new Runnable {
      def run = {
        UIManager.setLookAndFeel(new PlasticXPLookAndFeel)
        val frame = new PivotChartFrame(List(
          PivotChartViewTest.pivotTableDeltaByMonth,
          PivotChartViewTest.pivotTableDeltaWithDMinus1ByMonth,
          PivotChartViewTest.pivotTableDeltaByMonthAndMarket,
          PivotChartViewTest.pivotTableNDaysForwardCurve,
          PivotChartViewTest.pivotTableSpotFXPriceHistory))
        frame.pack
        frame.visible = true
      }
    })
  }
}


