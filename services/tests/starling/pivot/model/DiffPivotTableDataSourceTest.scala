package starling.pivot.model

import starling.utils.StarlingTest
import org.testng.annotations.Test
import org.testng.Assert._
import starling.pivot._

class DiffPivotTableDataSourceTest extends StarlingTest {

  @Test
  def testNoExtraFieldsAreAddedIfNoDataFields() {
    val ds = new PivotTableDataSource {
      def fieldDetailsGroups = List(FieldDetailsGroup("Test", List( FieldDetails("A"), FieldDetails("B"))))
      def data(pfs : PivotFieldsState) = {
        PivotResult(List(), Map())
      }
    }
    val diff = new DiffPivotTableDataSource(ds, ds, "B")
    assertEquals(ds.fieldDetails.size, diff.fieldDetails.size)
  }

  @Test
  def testExtraFieldsAreAddedForDataFields() {
    val ds = new PivotTableDataSource {
      def fieldDetailsGroups = List(FieldDetailsGroup("Test", List( FieldDetails("Name"), new SumIntFieldDetails("Height"))))
      def data(pfs : PivotFieldsState) = {
        PivotResult(List(), Map())
      }
    }
    val diff = new DiffPivotTableDataSource(ds, ds, "B")
    assertEquals(ds.fieldDetails.size + 3, diff.fieldDetails.size)
  }

  @Test
  def testSimpleDifference() {
    val dsA = new PivotTableDataSource {
      def fieldDetailsGroups = List(FieldDetailsGroup("Test", List( FieldDetails("Name"), new SumIntFieldDetails("Height"))))
      def data(pfs : PivotFieldsState) = {
        PivotResult(
          List( Map(Field("Name")->"Peter", Field("Height")->20) ),
          Map()
        )
      }
    }
    val dsB = new PivotTableDataSource {
      def fieldDetailsGroups = List(FieldDetailsGroup("Test", List( FieldDetails("Name"), new SumIntFieldDetails("Height"))))
      def data(pfs : PivotFieldsState) = {
        PivotResult(
          List( Map(Field("Name")->"Peter", Field("Height")->25) ),
          Map()
        )
      }
    }
    val diff = new DiffPivotTableDataSource(dsA, dsB, "B")
    val result = diff.data(PivotFieldsState(
      dataFields =  List(Field("Height"), Field("Height B"), Field("Height Change")),
      rowFields = List(Field("Name")),
      columnFields = List(),
      filters = List()
    ))
    assertEquals(2, result.data.size)
    val rowA = result.data(0)
    assertEquals("Peter", rowA(Field("Name")))
    assertEquals(20, rowA(Field("Height")))
    assertEquals(0, rowA(Field("Height B")))
    val rowB = result.data(1)
    assertEquals("Peter", rowB(Field("Name")))
    assertEquals(0, rowB(Field("Height")))
    assertEquals(25, rowB(Field("Height B")))

    assertEquals(DiffValue(true, 20), rowA(Field("Height Change")))
    assertEquals(DiffValue(false, 25), rowB(Field("Height Change")))
  }

  @Test
  def testAbsoluteDiffField {
    
  }
}
