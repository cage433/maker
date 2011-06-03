package starling.pivot.model

import starling.utils.StarlingTest
import org.testng.Assert._
import org.testng.annotations.{BeforeTest, Test}
import collection.mutable.ListBuffer

/**
 * User: brian
 * Date: 01-Dec-2009
 * Copyright (c) Trafigura 2009
 */

class DataAxisTest extends StarlingTest {
//  var ds: PivotResult = null
//
//  @BeforeTest
//  def initialise {
//    ds = new CannedDataSource().data(List(), List(), List(), List())
//  }
//
//  /**
//   * specify no fields in the axis
//   */
//  @Test
//  def specifyNoField {
//    val da = new DataAxis(List(), ds)
//    val ac = da.contents
//    assertEquals(0, ac.size, "Got the wrong number of columns")
//  }
//
//  /**
//   * specify only one field in the axis
//   */
//  @Test
//  def specifyOneField {
//    val da = new DataAxis(List(Field("Strike")), ds)
//    val ac = da.contents
//    assertEquals(3, ac.size, "Got the wrong number of columns")
//    val expectedStrikes = Set(80, 90, 100)
//    val strikes = Set() ++ ac.map(_.getValue)
//    assertEquals(strikes, expectedStrikes)
//  }
//
//  /**
//   * specify three fields in the axis
//   */
//  @Test
//  def specifyThreeFields {
//    val da = new DataAxis(List(Field("Strike"), Field("Trade"), Field("Expiry")), ds)
//    val ac = da.contents
//    assertEquals(3, ac.size, "Got the wrong number of columns")
//    val expectedStrikes = Set(80, 90, 100)
//    val strikes = Set() ++ ac.map(_.getValue)
//    assertEquals(strikes, expectedStrikes)
//  }
//
//  /**
//   * can we determine the predicates properly ?
//   */
//  @Test
//  def determinePredicates {
//    val predicates = new ListBuffer[List[Map[Field, AxisCellValue]]]
//    val f = Field("A")
//    val ac = new AxisColumn(f, 1)
//    ac.asPredicates(List(), predicates)
//    val p = predicates.toList
//    assertEquals(p, List(List(Map(f -> ShowAxisCellValue(1)))), "Got the wrong predicates")
//  }
//
//
//
//  @Test
//  def determinePredicatesFromAxis {
//    val da = new DataAxis(List(Field("Strike"), Field("Trade"), Field("Expiry")), ds)
//    val p = da.asPredicates
//    assertEquals(p.size, 5, "Got the wrong number of predicates, p="+p)
//   // p.foreach(pred => assertEquals(pred.size, 3, "Got the wrong number of predicates in " + pred))
//
//    // just check the first and last (non-sum)
//    val p1 = p(0)
//    assertEquals(Set() ++ p1, Set() ++ List(Map(Field("Strike") -> ShowAxisCellValue(100)), Map(Field("Trade") -> ShowAxisCellValue("T1")), Map(Field("Expiry") -> ShowAxisCellValue(0.5))), "Got the wrong predicates")
//  }
}
