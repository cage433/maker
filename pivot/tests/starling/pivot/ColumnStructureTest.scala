package starling.pivot

import org.scalatest.testng.TestNGSuite
import org.testng.annotations._
import org.testng.Assert._
import starling.pivot.ColumnStructure._

class ColumnStructureTest extends TestNGSuite {
  val cs = ColumnStructure(RootField, true, List(ColumnStructure(Field("Product"), true, List(
    ColumnStructure(Field("Trade"), true, List()),
    ColumnStructure(Field("Lots"), true, List(
      ColumnStructure(Field("Trader"), true, List()),
      ColumnStructure(Field("Expiry"), true, List(
        ColumnStructure(Field("Strike"), true, List()))))),
    ColumnStructure(Field("PV"), true, List())))))

  @Test
  def testContains() {
    assertTrue(cs.contains(Field("Product")))
    assertTrue(cs.contains(Field("Trade")))
    assertTrue(cs.contains(Field("Lots")))
    assertTrue(cs.contains(Field("Trader")))
    assertTrue(cs.contains(Field("Expiry")))
    assertTrue(cs.contains(Field("Strike")))
    assertTrue(cs.contains(Field("PV")))

    assertFalse(cs.contains(Field("Not There")))
  }

  @Test
  def testAddDataField() {
    val csRoot = ColumnStructure(RootField, true, List())
    val expectedRoot = ColumnStructure(RootField, true, List(ColumnStructure(Field("PV"), true, List())))
    val actualRoot = csRoot.addDataField(Field("PV"))
    assertEquals(actualRoot, expectedRoot)

    val csNonMeasure = ColumnStructure(RootField, true, List(ColumnStructure(Field("Lots"), false, List())))
    val expectedNonMeasure = ColumnStructure(RootField, true, List(ColumnStructure(Field("Lots"), false, List(
      ColumnStructure(Field("PV"), true, List())
      ))))
    val actualNonMeasure = csNonMeasure.addDataField(Field("PV"))
    assertEquals(actualNonMeasure, expectedNonMeasure)

    val csNonMeasureWithData = ColumnStructure(RootField, true, List(
      ColumnStructure(Field("Lots"), false, List(
        ColumnStructure(Field("PV"), true, List())
        ))))
    val expectedNonMeasureWithData = ColumnStructure(RootField, true, List(
      ColumnStructure(Field("Lots"), false, List(
        ColumnStructure(Field("PV"), true, List()),
        ColumnStructure(Field("Delta"), true, List())      
        ))))
    val actualNonMeasureWithData = csNonMeasureWithData.addDataField(Field("Delta"))
    assertEquals(actualNonMeasureWithData, expectedNonMeasureWithData)


    val csNonMeasureWithData2 = ColumnStructure(RootField, true, List(
      ColumnStructure(Field("Lots"), false, List(
        ColumnStructure(Field("PV"), true, List()),
        ColumnStructure(Field("Product"), false, List())
        ))))
    val expectedNonMeasureWithData2 = ColumnStructure(RootField, true, List(
      ColumnStructure(Field("Lots"), false, List(
        ColumnStructure(Field("PV"), true, List()),
        ColumnStructure(Field("Product"), false, List()),
        ColumnStructure(Field("Delta"), true, List())      
        ))))
    val actualNonMeasureWithData2 = csNonMeasureWithData2.addDataField(Field("Delta"))
    assertEquals(actualNonMeasureWithData2, expectedNonMeasureWithData2)
  }

  @Test
  def testRemoveTopField() {
    val newCS = cs.remove(Field("Product"))
    val expected = ColumnStructure(RootField, true, List(
      ColumnStructure(Field("Trade"), true, List()),
      ColumnStructure(Field("Lots"), true, List(
        ColumnStructure(Field("Trader"), true, List()),
        ColumnStructure(Field("Expiry"), true, List(
          ColumnStructure(Field("Strike"), true, List()))))),
      ColumnStructure(Field("PV"), true, List())))
    
    assertEquals(newCS, expected)
  }

  @Test
  def testRemoveBottomField() {
    val newCS = cs.remove(Field("Strike"))
    val expected = ColumnStructure(RootField, true, List(ColumnStructure(Field("Product"), true, List(
      ColumnStructure(Field("Trade"), true, List()),
      ColumnStructure(Field("Lots"), true, List(
        ColumnStructure(Field("Trader"), true, List()),
        ColumnStructure(Field("Expiry"), true, List()))),
      ColumnStructure(Field("PV"), true, List())))))

    assertEquals(newCS, expected)
  }

  @Test
  def removeSecondLevelField1() {
    val newCS = cs.remove(Field("Lots"))
    val expected = ColumnStructure(RootField, true, List(ColumnStructure(Field("Product"), true, List(
      ColumnStructure(Field("Trade"), true, List()),
      ColumnStructure(Field("Trader"), true, List()),
      ColumnStructure(Field("Expiry"), true, List(
        ColumnStructure(Field("Strike"), true, List()))),
      ColumnStructure(Field("PV"), true, List())))))
    
    assertEquals(newCS, expected)
  }

  @Test
  def removeSecondLevelField2() {
    val newCS = cs.remove(Field("Trade"))
    val expected = ColumnStructure(RootField, true, List(ColumnStructure(Field("Product"), true, List(
      ColumnStructure(Field("Lots"), true, List(
        ColumnStructure(Field("Trader"), true, List()),
        ColumnStructure(Field("Expiry"), true, List(
          ColumnStructure(Field("Strike"), true, List()))))),
      ColumnStructure(Field("PV"), true, List())))))

    assertEquals(newCS, expected)
  }

  @Test
  def removeSecondLevelField3() {
    val newCS = cs.remove(Field("PV"))
    val expected = ColumnStructure(RootField, true, List(ColumnStructure(Field("Product"), true, List(
      ColumnStructure(Field("Trade"), true, List()),
      ColumnStructure(Field("Lots"), true, List(
        ColumnStructure(Field("Trader"), true, List()),
        ColumnStructure(Field("Expiry"), true, List(
          ColumnStructure(Field("Strike"), true, List())))))))))

    assertEquals(newCS, expected)
  }

  @Test
  def removeMiddleLevelField() {
    val newCS = cs.remove(Field("Expiry"))
    val expected = ColumnStructure(RootField, true, List(ColumnStructure(Field("Product"), true, List(
      ColumnStructure(Field("Trade"), true, List()),
      ColumnStructure(Field("Lots"), true, List(
        ColumnStructure(Field("Trader"), true, List()),
        ColumnStructure(Field("Strike"), true, List()))),
      ColumnStructure(Field("PV"), true, List())))))

    assertEquals(newCS, expected)
  }

  @Test
  def testFlipIsData() {
    val expectedCS1 = ColumnStructure(RootField, true, List(ColumnStructure(Field("Product"), true, List(
      ColumnStructure(Field("Trade"), true, List()),
      ColumnStructure(Field("Lots"), false, List(
        ColumnStructure(Field("Trader"), true, List()),
        ColumnStructure(Field("Expiry"), true, List(
          ColumnStructure(Field("Strike"), true, List()))))),
      ColumnStructure(Field("PV"), true, List())))))

    val res1 = cs.flipIsData(Field("Lots"))
    assertEquals(res1, expectedCS1)

    val res2 = res1.flipIsData(Field("Lots"))
    assertEquals(res2, cs)

    val expectedCS2 = ColumnStructure(RootField, true, List(ColumnStructure(Field("Product"), true, List(
      ColumnStructure(Field("Trade"), true, List()),
      ColumnStructure(Field("Lots"), true, List(
        ColumnStructure(Field("Trader"), true, List()),
        ColumnStructure(Field("Expiry"), true, List(
          ColumnStructure(Field("Strike"), true, List()))))),
      ColumnStructure(Field("PV"), false, List())))))

    val res3 = cs.flipIsData(Field("PV"))
    assertEquals(res3, expectedCS2)
  }

  @Test
  def testRemoveAll() {
    val cs = ColumnStructure(RootField, false, List(
      ColumnStructure(Field("Slide1"), false, List(
        ColumnStructure(Field("Slide2"), false, List(
          ColumnStructure(Field("Position"), true, List())))))))
    val expected = ColumnStructure(RootField, false, List(
          ColumnStructure(Field("Position"), true, List())))

    assertEquals(cs.removeAllChildren(Set(Field("Slide1"), Field("Slide2"))), expected)
  }

  @Test
  def testFlippable() {
    val csToUse1 = ColumnStructure(RootField, false, List(
      ColumnStructure(Field("Product"), false, List(
        ColumnStructure(Field("PV"), true, List())))))

    val res1 = csToUse1.isInvalid
    assertEquals(res1, false)

    val csToUse2 = ColumnStructure(RootField, false, List(
      ColumnStructure(Field("Product"), false, List(
        ColumnStructure(Field("PV"), false, List())))))

    val res2 = csToUse2.isInvalid
    assertEquals(res2, false)

    val csToUse3 = ColumnStructure(RootField, false, List(
      ColumnStructure(Field("Product"), false, List(
        ColumnStructure(Field("Trade"), false, List()),
        ColumnStructure(Field("Lots"), false, List(
          ColumnStructure(Field("Trader"), false, List()),
          ColumnStructure(Field("Expiry"), false, List(
            ColumnStructure(Field("Strike"), false, List()))))),
        ColumnStructure(Field("PV"), true, List())))))

    val res3 = csToUse3.isInvalid
    assertEquals(res3, false)

    val csToUse4 = ColumnStructure(RootField, false, List(
      ColumnStructure(Field("Product"), true, List(
        ColumnStructure(Field("Trade"), false, List()),
        ColumnStructure(Field("Lots"), false, List(
          ColumnStructure(Field("Trader"), false, List()),
          ColumnStructure(Field("Expiry"), false, List(
            ColumnStructure(Field("Strike"), false, List()))))),
        ColumnStructure(Field("PV"), false, List())))))

    val res4 = csToUse4.isInvalid
    assertEquals(res4, false)

    val csToUse5 = ColumnStructure(RootField, false, List(
      ColumnStructure(Field("Product"), false, List(
        ColumnStructure(Field("Trade"), false, List()),
        ColumnStructure(Field("Lots"), false, List(
          ColumnStructure(Field("Trader"), false, List()),
          ColumnStructure(Field("Expiry"), true, List(
            ColumnStructure(Field("Strike"), false, List()))))),
        ColumnStructure(Field("PV"), true, List())))))

    val res5 = csToUse5.isInvalid
    assertEquals(res5, false)

    val csToUse6 = ColumnStructure(RootField, false, List(
      ColumnStructure(Field("Product"), false, List(
        ColumnStructure(Field("Trade"), false, List()),
        ColumnStructure(Field("Lots"), false, List(
          ColumnStructure(Field("Trader"), false, List()),
          ColumnStructure(Field("Expiry"), false, List(
            ColumnStructure(Field("Strike"), true, List()))))),
        ColumnStructure(Field("PV"), true, List())))))

    val res6 = csToUse6.isInvalid
    assertEquals(res6, false)

    val csToUse7 = ColumnStructure(RootField, false, List(
      ColumnStructure(Field("Product"), false, List(
        ColumnStructure(Field("Trade"), false, List()),
        ColumnStructure(Field("Lots"), true, List(
          ColumnStructure(Field("Trader"), false, List()),
          ColumnStructure(Field("Expiry"), false, List(
            ColumnStructure(Field("Strike"), false, List()))))),
        ColumnStructure(Field("PV"), true, List())))))

    val res7 = csToUse7.isInvalid
    assertEquals(res7, false)

    val csToUse8 = ColumnStructure(RootField, false, List(
      ColumnStructure(Field("Product"), true, List(
        ColumnStructure(Field("Trade"), false, List()),
        ColumnStructure(Field("Lots"), true, List(
          ColumnStructure(Field("Trader"), false, List()),
          ColumnStructure(Field("Expiry"), false, List(
            ColumnStructure(Field("Strike"), false, List()))))),
        ColumnStructure(Field("PV"), true, List())))))

    val res8 = csToUse8.isInvalid
    assertEquals(res8, true)

    val csToUse9 = ColumnStructure(RootField, false, List(
      ColumnStructure(Field("Product"), false, List(
        ColumnStructure(Field("Trade"), false, List()),
        ColumnStructure(Field("Lots"), true, List(
          ColumnStructure(Field("Trader"), false, List()),
          ColumnStructure(Field("Expiry"), true, List(
            ColumnStructure(Field("Strike"), true, List()))))),
        ColumnStructure(Field("PV"), false, List())))))

    val res9 = csToUse9.isInvalid
    assertEquals(res9, true)

    val csToUse10 = ColumnStructure(RootField, false, List(
      ColumnStructure(Field("Product"), false, List(
        ColumnStructure(Field("Trade"), false, List()),
        ColumnStructure(Field("Lots"), true, List(
          ColumnStructure(Field("Trader"), false, List()),
          ColumnStructure(Field("Expiry"), false, List(
            ColumnStructure(Field("Strike"), true, List()))))),
        ColumnStructure(Field("PV"), false, List())))))

    val res10 = csToUse10.isInvalid
    assertEquals(res10, true)
  }

  @Test
  def testEditable() {
    val pv = Field("PV")
    val product = Field("Product")
    val delta = Field("Delta")
    val lots = Field("Lots")
    val strike = Field("Strike")

    val c1 = ColumnStructure(RootField, true, List(
      ColumnStructure(pv, true, List(
        ColumnStructure(product, false, List())
      ))
    ))
    assertTrue(c1.hasPathContaining(Set(pv, product)))

    val c2 = ColumnStructure(RootField, true, List(
      ColumnStructure(pv, true, List()), ColumnStructure(product, false, List()))
    )
    assertFalse(c2.hasPathContaining(Set(pv, product)))

    val c3 = ColumnStructure(RootField, true, List(
      ColumnStructure(product, false, List()), ColumnStructure(pv, true, List()))
    )
    assertFalse(c3.hasPathContaining(Set(pv, product)))

    val c4 = ColumnStructure(RootField, true, List(
      ColumnStructure(product, false, List(
        ColumnStructure(pv, true, List())
      )))
    )
    assertTrue(c4.hasPathContaining(Set(pv, product)))

    val c6 = ColumnStructure(RootField, true, List(
      ColumnStructure(pv, true, List(
        ColumnStructure(product, false, List())
      )),
      ColumnStructure(lots, false, List()))
    )
    assertFalse(c6.hasPathContaining(Set(pv, product, lots)))

    val c8 = ColumnStructure(RootField, true, List(
      ColumnStructure(pv, true, List(
        ColumnStructure(product, false, List())
      ))
    ))
    assertFalse(c8.hasPathContaining(Set(pv, product, lots)))

    val c9 = ColumnStructure(RootField, true, List(
      ColumnStructure(product, false, List(
        ColumnStructure(pv, true, List()),
        ColumnStructure(lots, false, List())
      ))
    ))
    assertFalse(c9.hasPathContaining(Set(pv, product, lots)))

    val c10 = ColumnStructure(RootField, true, List(
      ColumnStructure(product, false, List()), ColumnStructure(pv, true, List())
    ))
    assertFalse(c10.hasPathContaining(Set(pv, product)))
  }

  @Test
  def testIsBottomNonMeasureField() {
    val c = ColumnStructure(RootField, true, List(
      ColumnStructure(Field("PV"), true, List(
        ColumnStructure(Field("Product"), false, List())
      ))
    ))

    assertTrue(c.isBottomField(Field("Product")))

    val c1 = ColumnStructure(RootField, true, List(
      ColumnStructure(Field("Product"), false, List(
        ColumnStructure(Field("PV"), true, List())
      ))
    ))

    assertTrue(c1.isBottomField(Field("PV")))

    val c2 = ColumnStructure(RootField, true, List(
      ColumnStructure(Field("PV"), true, List(
        ColumnStructure(Field("Product"), false, List(
          ColumnStructure(Field("Lots"), false, List())
        ))
      ))
    ))

    assertTrue(c2.isBottomField(Field("Lots")))
    assertFalse(c2.isBottomField(Field("Product")))


    val c3 = ColumnStructure(RootField, true, List(
      ColumnStructure(Field("PV"), true, List(
        ColumnStructure(Field("Product"), false, List(
          ColumnStructure(Field("Lots"), false, List())
        )),
        ColumnStructure(Field("Other"), false, List())
      ))
    ))

    assertTrue(c3.isBottomField(Field("Lots")))
    assertTrue(c3.isBottomField(Field("Other")))
    assertFalse(c3.isBottomField(Field("PV")))
    assertFalse(c3.isBottomField(Field("Product")))
  }
}