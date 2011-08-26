package starling.pivot

import org.scalatest.testng.TestNGSuite
import org.testng.annotations._
import org.testng.Assert._

class PivotFieldsStateTests extends TestNGSuite {

  @Test
  def testRotate() {
    val pfs = new PivotFieldsState(rowFields=List(Field("A"), Field("B")), columns=ColumnTrees.createFlat(List(Field("X"), Field("Y")), List(Field("i"))))
    val rotated = pfs.rotate
    val expected = new PivotFieldsState(rowFields=List(Field("X"), Field("Y")), columns=ColumnTrees.createFlat(List(Field("A"), Field("B")), List(Field("i"))))
    assertEquals(rotated, expected)
  }

  @Test
  def moveField() {
    val start = new PivotFieldsState(filters = (Field("A") -> SomeSelection(Set("v1"))) :: Nil)
    val moveToRow = start.moveField(Field("A"), FieldChooserType.Filter, FieldChooserType.Rows, 0)
    val moveBack = moveToRow.moveField(Field("A"), FieldChooserType.Rows, FieldChooserType.Filter, 0)

    assertEquals(start, moveBack)
  }

  @Test
  def testAllFilterPathsWithComplexColumns() {
    val pfs = new PivotFieldsState(
      rowFields = List(Field("Risk Market")),
      columns = ColumnTrees(List(
        ColumnTree(Field("Position"), true),
        ColumnTree(Field("Day Change"), true,
          ColumnTree(Field("Day Change Component"), false)
        ))
      ),
      filters = List(Field("Day Change Component") -> SomeSelection(Set("n/a"))))

    val expected = FiltersList(List(
      List(Field("Risk Market") -> SelectionPossibleValuesFilter(AllSelection)),
      List(Field("Risk Market") -> SelectionPossibleValuesFilter(AllSelection)),
      List(Field("Risk Market") -> SelectionPossibleValuesFilter(AllSelection), Field("Day Change Component") -> SelectionPossibleValuesFilter(SomeSelection(Set("n/a"))))
    ))
    assertEquals(pfs.allFilterPaths, expected)
  }

  @Test
  def testAllFilterPathsWithFilterAreaFilter() {
    val pfs = new PivotFieldsState(
      rowFields = List(Field("Risk Market")),
      columns = ColumnTrees(List(
        ColumnTree(Field("Position"), true),
        ColumnTree(Field("Day Change"), true,
          ColumnTree(Field("Day Change Component"), false)
        )
      )),
      filters = List(Field("Instrument") -> SomeSelection(Set("Future"))))

    val expected = FiltersList(List(
      List(Field("Instrument") -> SelectionPossibleValuesFilter(SomeSelection(Set("Future"))), Field("Risk Market") -> SelectionPossibleValuesFilter(AllSelection)),
      List(Field("Instrument") -> SelectionPossibleValuesFilter(SomeSelection(Set("Future"))), Field("Risk Market") -> SelectionPossibleValuesFilter(AllSelection)),
      List(Field("Instrument") -> SelectionPossibleValuesFilter(SomeSelection(Set("Future"))), Field("Risk Market") -> SelectionPossibleValuesFilter(AllSelection), Field("Day Change Component") -> SelectionPossibleValuesFilter(AllSelection))
    ))
    assertEquals(pfs.allFilterPaths, expected)
  }

  @Test
  def testAllFilterPathsWithNothing() {
    val pfs = PivotFieldsState(dataFields=List(Field("Position")))
    val expected = FiltersList(List(
      List()
    ))
    assertEquals(pfs.allFilterPaths, expected)
  }

  @Test
  def testAllFilterPathsNoRows() {
    val pfs = new PivotFieldsState(
      columns = ColumnTrees(List(
        ColumnTree(Field("Position"), true),
        ColumnTree(Field("Day Change"), true,
          ColumnTree(Field("Day Change Component"), false)
        ))
      ),
      filters = List(Field("Day Change Component") -> SomeSelection(Set("n/a"))))

    val expected = FiltersList(List(
      List(),
      List(Field("Day Change Component") -> SelectionPossibleValuesFilter(SomeSelection(Set("n/a"))))
    ))
    assertEquals(pfs.allFilterPaths, expected)
  }

}