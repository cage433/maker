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

  @Test
  def testTreeBuilderOneRowOneColumn{
    val grid = Array(
      Array( av("a"))
    )
    val nodes = AxisNodeBuilder.createNodes(grid, 0, grid.size, 0)
    assertEquals(nodes, List(AxisNode(av("a"), List())))
  }

  @Test
  def testTreeBuilderOneRowTwoColumns {
    val grid = Array(
      Array( av("a"), av("b"))
    )
    val nodes = AxisNodeBuilder.createNodes(grid, 0, grid.size, 0)
    assertEquals(nodes, List(AxisNode(av("a"), List(AxisNode(av("b"), List())))))
  }

  @Test
  def testTreeBuilderOneColumnTwoRows {
    val grid = Array(
      Array( av("X") ),
      Array( av("Y") )
    )
    val nodes = AxisNodeBuilder.createNodes(grid, 0, grid.size, 0)
    assertEquals(nodes, List( AxisNode(av("X"), List()), AxisNode(av("Y"), List()) ))
  }

  @Test
  def testTreeBuilderWithSimpleTree {
    val grid = Array(
      Array( av("a"), av("X"), av("d1") ),
      Array( av("a"), av("Y"), av("d1") )
    )
    val nodes = AxisNodeBuilder.createNodes(grid, 0, grid.size, 0)
    assertEquals(nodes, List(
      AxisNode(av("a"), List(
        AxisNode(av("X"), List( AxisNode(av("d1"),List()) )),
        AxisNode(av("Y"), List( AxisNode(av("d1"),List()) ))
      ))
    ))
  }

//  val b = List(
//  AxisNode(AxisValue(Field(F),ValueAxisValueType(a,a),0),List(
//    AxisNode(AxisValue(Field(F),ValueAxisValueType(X,X),0),List(
//      AxisNode(AxisValue(Field(F),ValueAxisValueType(d1,d1),0),List()))),
//    AxisNode(AxisValue(Field(F),ValueAxisValueType(Y,Y),0),List(
//      AxisNode(AxisValue(Field(F),ValueAxisValueType(d1,d1),0),List()),
//      AxisNode(AxisValue(Field(F),ValueAxisValueType(d2,d2),0),List())
//    ))
//  )),
//  AxisNode(AxisValue(Field(F),ValueAxisValueType(b,b),0),List(
//    AxisNode(AxisValue(Field(F),ValueAxisValueType(X,X),0),List(
//      AxisNode(AxisValue(Field(F),ValueAxisValueType(d1,d1),0),List()))),
//    AxisNode(AxisValue(Field(F),ValueAxisValueType(Y,Y),0),List(
//      AxisNode(AxisValue(Field(F),ValueAxisValueType(d1,d1),0),List()),
//      AxisNode(AxisValue(Field(F),ValueAxisValueType(d2,d2),0),List()))),
//    AxisNode(AxisValue(Field(F),ValueAxisValueType(X,X),0),List(
//      AxisNode(AxisValue(Field(F),ValueAxisValueType(d1,d1),0),List()),
//      AxisNode(AxisValue(Field(F),ValueAxisValueType(d2,d2),0),List()),
//      AxisNode(AxisValue(Field(F),ValueAxisValueType(d1,d1),0),List())
//    ))
//  ))
//)>


  @Test
  def testTreeBuilderManyRowsAndColumns {
    val grid = Array(
      Array( av("a"), av("X"), av("d1") ),
      Array( av("a"), av("Y"), av("d1") ),
      Array( av("a"), av("Y"), av("d2") ),
      Array( av("b"), av("X"), av("d1") )
    )
    val nodes = AxisNodeBuilder.createNodes(grid, 0, grid.size, 0)
    assertEquals(nodes, List(
      AxisNode(av("a"), List(
        AxisNode(av("X"), List(AxisNode(av("d1"), List()))),
        AxisNode(av("Y"), List(AxisNode(av("d1"), List()), AxisNode(av("d2"), List())))
      )),
      AxisNode(av("b"), List(
        AxisNode(av("X"), List(AxisNode(av("d1"), List())))
      ))
    ))
  }

  @Test
  def testTreeBuilderEmpty {
    val grid = Array[Array[AxisValue]]()
    val nodes = AxisNodeBuilder.createNodes(grid, 0, grid.size, 0)
    assertEquals(nodes, List())
  }
}