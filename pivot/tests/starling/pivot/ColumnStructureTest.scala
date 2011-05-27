package starling.pivot

import org.scalatest.testng.TestNGSuite
import org.testng.annotations._
import org.testng.Assert._
import starling.pivot.ColumnTrees._
import starling.pivot.PivotFieldsState._

class ColumnStructureTest extends TestNGSuite {
  val cs = ColumnTrees(
      Field("Product"), true,
      List(
        ColumnTree(Field("Trade"), true),
        ColumnTree(Field("Lots"), true, 
          ColumnTree(Field("Trader"), true),
          ColumnTree(Field("Expiry"), true, 
            ColumnTree(Field("Strike"), true)
          )
        ),
        ColumnTree(Field("PV"), true)
      ))

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
    val csRoot = ColumnTrees()
    val expectedRoot = ColumnTrees(ColumnTree(Field("PV"), true))
    val actualRoot = csRoot.addDataField(Field("PV"))
    assertEquals(actualRoot, expectedRoot)

    val csNonMeasure = ColumnTrees(List(ColumnTree(Field("Lots"), false)))
    val expectedNonMeasure = ColumnTrees(List(
      ColumnTree(Field("Lots"), false),
      ColumnTree(Field("PV"), true)))
    val actualNonMeasure = csNonMeasure.addDataField(Field("PV"))
    assertEquals(actualNonMeasure, expectedNonMeasure)

    val csNonMeasureWithData = ColumnTrees(List(
      ColumnTree(Field("Lots"), false,
        ColumnTree(Field("PV"), true)
        )))
    val expectedNonMeasureWithData = ColumnTrees(List(
      ColumnTree(Field("Lots"), false,
        ColumnTree(Field("PV"), true)
        ), ColumnTree(Field("Delta"), true)))
    val actualNonMeasureWithData = csNonMeasureWithData.addDataField(Field("Delta"))
    assertEquals(actualNonMeasureWithData, expectedNonMeasureWithData)


    val csNonMeasureWithData2 = ColumnTrees(List(
      ColumnTree(Field("Lots"), false,
        ColumnTree(Field("PV"), true),
        ColumnTree(Field("Product"), false)
        )))
    val expectedNonMeasureWithData2 = ColumnTrees(List(
      ColumnTree(Field("Lots"), false,
        ColumnTree(Field("PV"), true),
        ColumnTree(Field("Product"), false)
        ), ColumnTree(Field("Delta"), true)))
    val actualNonMeasureWithData2 = csNonMeasureWithData2.addDataField(Field("Delta"))
    assertEquals(actualNonMeasureWithData2, expectedNonMeasureWithData2)
  }

  @Test
  def testRemoveTopField() {
    val newCS = cs.remove(Field("Product"))
    val expected = ColumnTrees(List(
      ColumnTree(Field("Trade"), true),
      ColumnTree(Field("Lots"), true,
        ColumnTree(Field("Trader"), true),
        ColumnTree(Field("Expiry"), true,
          ColumnTree(Field("Strike"), true))),
      ColumnTree(Field("PV"), true)))
    
    assertEquals(newCS, expected)
  }

  @Test
  def testRemoveBottomField() {
    val newCS = cs.remove(Field("Strike"))
    val expected = ColumnTrees(List(ColumnTree(Field("Product"), true,
      ColumnTree(Field("Trade"), true),
      ColumnTree(Field("Lots"), true,
        ColumnTree(Field("Trader"), true),
        ColumnTree(Field("Expiry"), true)),
      ColumnTree(Field("PV"), true))))

    assertEquals(newCS, expected)
  }

  @Test
  def removeSecondLevelField1() {
    val newCS = cs.remove(Field("Lots"))
    val expected = ColumnTrees(List(ColumnTree(Field("Product"), true,
      ColumnTree(Field("Trade"), true),
      ColumnTree(Field("Trader"), true),
      ColumnTree(Field("Expiry"), true,
        ColumnTree(Field("Strike"), true)),
      ColumnTree(Field("PV"), true))))
    
    assertEquals(newCS, expected)
  }

  @Test
  def removeSecondLevelField2() {
    val newCS = cs.remove(Field("Trade"))
    val expected = ColumnTrees(List(ColumnTree(Field("Product"), true,
      ColumnTree(Field("Lots"), true,
        ColumnTree(Field("Trader"), true),
        ColumnTree(Field("Expiry"), true,
          ColumnTree(Field("Strike"), true))),
      ColumnTree(Field("PV"), true))))

    assertEquals(newCS, expected)
  }

  @Test
  def removeSecondLevelField3() {
    val newCS = cs.remove(Field("PV"))
    val expected = ColumnTrees(List(ColumnTree(Field("Product"), true,
      ColumnTree(Field("Trade"), true),
      ColumnTree(Field("Lots"), true,
        ColumnTree(Field("Trader"), true),
        ColumnTree(Field("Expiry"), true,
          ColumnTree(Field("Strike"), true))))))

    assertEquals(newCS, expected)
  }

  @Test
  def removeMiddleLevelField() {
    val newCS = cs.remove(Field("Expiry"))
    val expected = ColumnTrees(List(ColumnTree(Field("Product"), true,
      ColumnTree(Field("Trade"), true),
      ColumnTree(Field("Lots"), true,
        ColumnTree(Field("Trader"), true),
        ColumnTree(Field("Strike"), true)),
      ColumnTree(Field("PV"), true))))

    assertEquals(newCS, expected)
  }

  @Test
  def testFlipIsData() {
    val expectedCS1 = ColumnTrees(List(ColumnTree(Field("Product"), true,
      ColumnTree(Field("Trade"), true),
      ColumnTree(Field("Lots"), false,
        ColumnTree(Field("Trader"), true),
        ColumnTree(Field("Expiry"), true,
          ColumnTree(Field("Strike"), true))),
      ColumnTree(Field("PV"), true))))

    val res1 = cs.flipIsData(Field("Lots"))
    assertEquals(res1, expectedCS1)

    val res2 = res1.flipIsData(Field("Lots"))
    assertEquals(res2, cs)

    val expectedCS2 = ColumnTrees(List(ColumnTree(Field("Product"), true,
      ColumnTree(Field("Trade"), true),
      ColumnTree(Field("Lots"), true,
        ColumnTree(Field("Trader"), true),
        ColumnTree(Field("Expiry"), true,
          ColumnTree(Field("Strike"), true))),
      ColumnTree(Field("PV"), false))))

    val res3 = cs.flipIsData(Field("PV"))
    assertEquals(res3, expectedCS2)
  }

  @Test
  def testRemoveAll() {
    val cs = ColumnTrees(List(
      ColumnTree(Field("Slide1"), false,
        ColumnTree(Field("Slide2"), false,
          ColumnTree(Field("Position"), true)))))
    val expected = ColumnTrees(List(
          ColumnTree(Field("Position"), true)))

    assertEquals(cs.removeAllChildren(Set(Field("Slide1"), Field("Slide2"))), expected)
  }

  @Test
  def testFlippable() {
    val csToUse1 = ColumnTrees(List(
      ColumnTree(Field("Product"), false,
        ColumnTree(Field("PV"), true))))

    val res1 = csToUse1.isInvalid
    assertEquals(res1, false)

    val csToUse2 = ColumnTrees(List(
      ColumnTree(Field("Product"), false,
        ColumnTree(Field("PV"), false))))

    val res2 = csToUse2.isInvalid
    assertEquals(res2, false)

    val csToUse3 = ColumnTrees(List(
      ColumnTree(Field("Product"), false,
        ColumnTree(Field("Trade"), false),
        ColumnTree(Field("Lots"), false,
          ColumnTree(Field("Trader"), false),
          ColumnTree(Field("Expiry"), false,
            ColumnTree(Field("Strike"), false))),
        ColumnTree(Field("PV"), true))))

    val res3 = csToUse3.isInvalid
    assertEquals(res3, false)

    val csToUse4 = ColumnTrees(List(
      ColumnTree(Field("Product"), true,
        ColumnTree(Field("Trade"), false),
        ColumnTree(Field("Lots"), false,
          ColumnTree(Field("Trader"), false),
          ColumnTree(Field("Expiry"), false,
            ColumnTree(Field("Strike"), false))),
        ColumnTree(Field("PV"), false))))

    val res4 = csToUse4.isInvalid
    assertEquals(res4, false)

    val csToUse5 = ColumnTrees(List(
      ColumnTree(Field("Product"), false,
        ColumnTree(Field("Trade"), false),
        ColumnTree(Field("Lots"), false,
          ColumnTree(Field("Trader"), false),
          ColumnTree(Field("Expiry"), true,
            ColumnTree(Field("Strike"), false))),
        ColumnTree(Field("PV"), true))))

    val res5 = csToUse5.isInvalid
    assertEquals(res5, false)

    val csToUse6 = ColumnTrees(List(
      ColumnTree(Field("Product"), false,
        ColumnTree(Field("Trade"), false),
        ColumnTree(Field("Lots"), false,
          ColumnTree(Field("Trader"), false),
          ColumnTree(Field("Expiry"), false,
            ColumnTree(Field("Strike"), true))),
        ColumnTree(Field("PV"), true))))

    val res6 = csToUse6.isInvalid
    assertEquals(res6, false)

    val csToUse7 = ColumnTrees(List(
      ColumnTree(Field("Product"), false,
        ColumnTree(Field("Trade"), false),
        ColumnTree(Field("Lots"), true,
          ColumnTree(Field("Trader"), false),
          ColumnTree(Field("Expiry"), false,
            ColumnTree(Field("Strike"), false))),
        ColumnTree(Field("PV"), true))))

    val res7 = csToUse7.isInvalid
    assertEquals(res7, false)

    val csToUse8 = ColumnTrees(List(
      ColumnTree(Field("Product"), true,
        ColumnTree(Field("Trade"), false),
        ColumnTree(Field("Lots"), true,
          ColumnTree(Field("Trader"), false),
          ColumnTree(Field("Expiry"), false,
            ColumnTree(Field("Strike"), false))),
        ColumnTree(Field("PV"), true))))

    val res8 = csToUse8.isInvalid
    assertEquals(res8, true)

    val csToUse9 = ColumnTrees(List(
      ColumnTree(Field("Product"), false,
        ColumnTree(Field("Trade"), false),
        ColumnTree(Field("Lots"), true,
          ColumnTree(Field("Trader"), false),
          ColumnTree(Field("Expiry"), true,
            ColumnTree(Field("Strike"), true))),
        ColumnTree(Field("PV"), false))))

    val res9 = csToUse9.isInvalid
    assertEquals(res9, true)

    val csToUse10 = ColumnTrees(List(
      ColumnTree(Field("Product"), false,
        ColumnTree(Field("Trade"), false),
        ColumnTree(Field("Lots"), true,
          ColumnTree(Field("Trader"), false),
          ColumnTree(Field("Expiry"), false,
            ColumnTree(Field("Strike"), true))),
        ColumnTree(Field("PV"), false))))

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

    val c1 = ColumnTrees(List(
      ColumnTree(pv, true,
        ColumnTree(product, false)
      ))
    )
    assertTrue(c1.hasPathContaining(Set(pv, product)))

    val c2 = ColumnTrees(List(
      ColumnTree(pv, true), ColumnTree(product, false))
    )
    assertFalse(c2.hasPathContaining(Set(pv, product)))

    val c3 = ColumnTrees(List(
      ColumnTree(product, false), ColumnTree(pv, true))
    )
    assertFalse(c3.hasPathContaining(Set(pv, product)))

    val c4 = ColumnTrees(List(
      ColumnTree(product, false,
        ColumnTree(pv, true)
      ))
    )
    assertTrue(c4.hasPathContaining(Set(pv, product)))

    val c6 = ColumnTrees(List(
      ColumnTree(pv, true,
        ColumnTree(product, false)
      ),
      ColumnTree(lots, false))
    )
    assertFalse(c6.hasPathContaining(Set(pv, product, lots)))

    val c8 = ColumnTrees(List(
      ColumnTree(pv, true,
        ColumnTree(product, false)
      )
    ))
    assertFalse(c8.hasPathContaining(Set(pv, product, lots)))

    val c9 = ColumnTrees(List(
      ColumnTree(product, false,
        ColumnTree(pv, true),
        ColumnTree(lots, false)
      )
    ))
    assertFalse(c9.hasPathContaining(Set(pv, product, lots)))

    val c10 = ColumnTrees(List(
      ColumnTree(product, false), ColumnTree(pv, true)
    ))
    assertFalse(c10.hasPathContaining(Set(pv, product)))
  }

  @Test
  def testBuildPathsFor() {
    val c1 = ColumnTrees(List(
      ColumnTree(Field("PV"), true,
        ColumnTree(Field("Product"), false)
      )
    ))

    val expected1 = List(
      ColumnStructurePath(Some(Field("PV")),List((Field("PV"),0), (Field("Product"),0)))
    )
    val result1 = c1.buildPaths()
    assertEquals(result1, expected1)

    val c2 = ColumnTrees(List(
      ColumnTree(Field("PV"), true,
        ColumnTree(Field("Product"), false)
      ),
      ColumnTree(Field("Gamma"), true)
    ))

    val expected2 = List(
      ColumnStructurePath(Some(Field("PV")),List((Field("PV"),0), (Field("Product"),0))),
      ColumnStructurePath(Some(Field("Gamma")),List((Field("Gamma"), 1)))
    )
    val result2 = c2.buildPaths()
    assertEquals(result2, expected2)


    val c31 = ColumnTrees(List(
      ColumnTree(Field("PV"), true), ColumnTree(Field("Gamma"), true)
    ))
    val c3 = ColumnTrees(List(
      ColumnTree(FieldOrColumnStructure(c31),
        ColumnTrees(ColumnTree(Field("Product"), false))
      )
    ))
    val expected3 = List(
      ColumnStructurePath(Some(Field("PV")),List((Field("PV"),0), (Field("Product"),0))),
      ColumnStructurePath(Some(Field("Gamma")),List((Field("Gamma"),1), (Field("Product"),0)))
    )
    val result3 = c3.buildPaths()
    assertEquals(result3, expected3)


    val c41 = ColumnTrees(List(
      ColumnTree(Field("PV"), true,
        ColumnTree(Field("Strike"), false)
      ),
      ColumnTree(Field("Gamma"), true)
    ))

    val c4 = ColumnTrees(List(
      ColumnTree(FieldOrColumnStructure(c41),
        ColumnTrees(ColumnTree(Field("Product"), false))
    )))
    val expected4 = List(
      ColumnStructurePath(Some(Field("PV")),List((Field("PV"),0), (Field("Strike"), 0), (Field("Product"),0))),
      ColumnStructurePath(Some(Field("Gamma")),List((Field("Gamma"),1), (Field("Product"),0)))
    )
    val result4 = c4.buildPaths()
    assertEquals(result4, expected4)


    val c51 = ColumnTrees(List(
      ColumnTree(Field("PV"), true,
        ColumnTree(Field("Strike"), false)
      ),
      ColumnTree(Field("Gamma"), true)
    ))

    val c5 = ColumnTrees(List(
      ColumnTree(FieldOrColumnStructure(c51),
        ColumnTrees(ColumnTree(Field("Product"), false))
      ),
      ColumnTree(Field("Delta"), true))
    )
    val expected5 = List(
      ColumnStructurePath(Some(Field("PV")),List((Field("PV"),0), (Field("Strike"), 0), (Field("Product"),0))),
      ColumnStructurePath(Some(Field("Gamma")),List((Field("Gamma"),1), (Field("Product"),0))),
      ColumnStructurePath(Some(Field("Delta")),List((Field("Delta"), 2)))
    )
    val result5 = c5.buildPaths()
    assertEquals(result5, expected5)
  }

  @Test
  def testRemove01() {
    val newCS = ColumnTrees(
      ColumnTree(
        FieldOrColumnStructure(
          ColumnTrees(
            List(ColumnTree(Field("PV"), true), ColumnTree(Field("Gamma"), true))
          )
        ), ColumnTrees.Null
      )
    )
    val expectedCS = ColumnTrees(
      ColumnTree(
        Field("PV"), true
      )
    )
    val result = newCS.remove(Field("Gamma"))
    assertEquals(result, expectedCS)
  }

  @Test
  def testBuildPathsWithPadding() {
    val unevenTree = ColumnTrees(List(
      ColumnTree(Field("PV"), true), ColumnTree(Field("Gamma"), true, ColumnTrees(ColumnTree(Field("Expiry"), false)))
    ))
    val columnTrees = ColumnTrees(List(ColumnTree(FieldOrColumnStructure(Right(unevenTree)), ColumnTrees(Field("Product"), false))))

    val expected = List(ColumnStructurePath(Some(Field("PV")),List((Field("PV"),0), (Field.NullField,0), (Field("Product"),0))),
      ColumnStructurePath(Some(Field("Gamma")),List((Field("Gamma"),1), (Field("Expiry"),0), (Field("Product"),0))))

    val result = columnTrees.buildPathsWithPadding
    assertEquals(result, expected)
  }
}