package starling.pivot

import controller.{AxisNode, AxisNodeBuilder, PivotTableConverter}
import model._
import org.testng.annotations.Test
import org.testng.Assert._
import starling.pivot._
import org.scalatest.testng.TestNGSuite
import starling.pivot.ColumnTrees._

/**
 */

class AxisTreeTester extends TestNGSuite {

  private def av(value:Any) = AxisValue(Field("F"), ValueAxisValueType(value), 0)
  val formatInfo = {
    val m = Map(Field("F") -> DefaultPivotFormatter)
    FormatInfo(m)
  }

  @Test
  def testFlattenSingle {
    val node = AxisNode(av("P"), List())
    val flatten = node.flatten(List(), false, false, CollapsedState.None, List(), formatInfo, PivotFormatter.DefaultExtraFormatInfo)
    assertEquals(flatten, List(List(AxisCell(av("P"), Some(1), "P", None, false, NotTotal, 0, LeftTextPosition))))
  }

  @Test
  def testFlattenChildren {
    val node = AxisNode(av("P"), List( AxisNode(av("c1"), List()), AxisNode(av("c2"), List())))
    val flatten = node.flatten(List(), false, false, CollapsedState.None, List(), formatInfo, PivotFormatter.DefaultExtraFormatInfo)
    assertEquals(
      flatten,
      List(
        List(AxisCell(av("P"), Some(2), "P", Some(false), false, NotTotal, 0, LeftTextPosition),
          AxisCell(av("c1"), Some(1), "c1", None, false, NotTotal, 0, LeftTextPosition)),
        List(AxisCell(av("P"), None, "P", None, true, NotTotal, 1, LeftTextPosition),
          AxisCell(av("c2"), Some(1), "c2", None, false, NotTotal, 0, LeftTextPosition))
      )
    )
  }
}