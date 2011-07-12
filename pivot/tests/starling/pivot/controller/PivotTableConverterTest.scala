package starling.pivot.controller

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._
import starling.pivot.model.{AxisValue, ValueAxisValueType}
import starling.pivot.Field

class PivotTableConverterTest extends TestNGSuite {
  private def av(value:Any) = AxisValue(Field("F"), ValueAxisValueType(value), 0)

  implicit def convert(axisValue:AxisValue):ChildKey = axisValue.keyValue

  @Test
  def testPurge() {
    val an = AxisNode(av("bla"), Nil)
    val res1 = an.purge(Set())
    assertEquals(res1, Some(an))

    val res2 = an.purge(Set(List(av("bla"))))
    assertEquals(res2, None)

    val an1 = AxisNode(av("bla1"), List(an, AxisNode(av("bla2"), Nil)))
    val expected = AxisNode(av("bla1"), List(AxisNode(av("bla2"), Nil)))
    val res3 = an1.purge(Set(List(av("bla1"), av("bla"))))
    assertEquals(res3, Some(expected))

    val an2 = AxisNode(av("bla1"), List(an))
    val res4 = an2.purge(Set(List(av("bla1"), av("bla"))))
    assertEquals(res4, None)

    val an3 = AxisNode(av("bla1"), List(
      AxisNode(av("bla2"), List(AxisNode(av("bla2.5"), List(AxisNode(av("bla2.55"), Nil))))),
      AxisNode(av("bla3"), List(AxisNode(av("bla3.5"), List(AxisNode(av("bla3.55"), Nil)))))
    ))
    val res5 = an3.purge(Set(List(av("bla3"), av("bla3.5"), av("bla3.55"))))
    assertEquals(res5, Some(an3))
    val res6 = an3.purge(Set(List(av("bla1"), av("bla3"), av("bla3.5"), av("bla3.55"))))
    val expected1 = AxisNode(av("bla1"), List(
      AxisNode(av("bla2"), List(AxisNode(av("bla2.5"), List(AxisNode(av("bla2.55"), Nil)))))
    ))
    assertEquals(res6, Some(expected1))

    val res7 = an3.purge(Set(List(av("bla1"), av("bla2"), av("bla2.5"), av("bla2.55"))))
    val expected2 = AxisNode(av("bla1"), List(
      AxisNode(av("bla3"), List(AxisNode(av("bla3.5"), List(AxisNode(av("bla3.55"), Nil)))))
    ))
    assertEquals(res7, Some(expected2))
  }
}