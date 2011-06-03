package starling.pivot.model

import starling.utils.StarlingTest
import org.testng.annotations.Test
import org.testng.Assert._
import starling.pivot._
import controller.PivotTableConverter
import org.scalatest.testng.TestNGSuite
import starling.pivot.ColumnTrees._

/**
 */

class PivotTableTest extends TestNGSuite {

  val pivotTableDataSource = new UnfilteredPivotTableDataSource() {
    def fieldDetailsGroups = List( FieldDetailsGroup("", List(
      FieldDetails("Market"), FieldDetails("Period"), new SumIntFieldDetails("Delta"), FieldDetails("Day Change Component"), new SumIntFieldDetails("Day Change")
    )) )

    def unfilteredData(pfs: PivotFieldsState) = {
      List(
        Map(Field("Market") -> "WTI",    Field("Period") -> "Jan", Field("Delta") -> 1),
        Map(Field("Market") -> "WTI",    Field("Period") -> "Feb", Field("Delta") -> 7),
        Map(Field("Market") -> "Brent",  Field("Period") -> "Jan", Field("Delta") -> 4),
        Map(Field("Market") -> "Unused"),
        Map(Field("Market") -> "Brent",  Field("Period") -> "Jan", Field("Day Change Component") -> "Price", Field("Day Change") -> 100),
        Map(Field("Market") -> "Brent",  Field("Period") -> "Jan", Field("Day Change Component") -> "Vol", Field("Day Change") -> 18)
      )
    }
  }

  @Test
  def testSimpleRowAndData {
    check(
      pivotTableDataSource,
      PivotFieldsState(dataFields=List(Field("Delta")), rowFields=List(Field("Market"))),
      Totals.Null,
      List( List("Brent"), List("WTI") ), //No 'Unused' because it has no delta value
      List( List(""), List("Delta") ),
      List( List( 4 ), List(1+7) )
    )
  }

  @Test
  def testNoRowValueForDataField {
    check(
      pivotTableDataSource,
      PivotFieldsState(dataFields=List(Field("Delta")), rowFields=List(Field("Day Change Component"))),
      Totals.Null,
      List( List("n/a") ),
      List( List(""), List("Delta") ),
      List( List( 1+7+4 ) )
    )
  }

  @Test
  def testNoColumnValueForDataField {
    check(
      pivotTableDataSource,
      PivotFieldsState(dataFields=List(Field("Delta")), columnFields=List(Field("Day Change Component"))),
      Totals.Null,
      List( List("") ),
      List( List("n/a"),
           List("Delta") ),
      List( List( 1+7+4 ) )
    )
  }

  @Test
  def abc() {
    def permutations(list:List[Any]):List[List[Any]] = {
      list match {
        case Nil => List(Nil)
        case head :: tail => {
          val rest = permutations(tail)
          rest.flatMap(l => List("Total" :: l, head :: l))
        }
      }
    }
    permutations(List("WTI", "Feb", "Delta")).foreach(println)
  }

  @Test
  def testColumnAndData {
    check(
      pivotTableDataSource,
      PivotFieldsState(dataFields=List(Field("Delta")), columnFields=List(Field("Market"))),
      Totals.Null,
      List( List("") ),
      List( List("Brent", "WTI"),   //No 'Unused Market' because it has no delta value
            List("Delta", "Delta") ),
      List( List( 4, 1+7 ) )
    )
  }

  @Test
  def testColumnAndDataWithColumnSubTotalsWithMeasureFirst {
    check(
      pivotTableDataSource,
      new PivotFieldsState(columns=
        ColumnTrees(
          List(ColumnTree(Field("Delta"), true,
            ColumnTree(Field("Market"), false,
              ColumnTree(Field("Period"), false)))))),
      Totals(false, false, false, true),
      List( List("") ),
      List( List("Delta", "Delta", "Delta", "Delta", "Delta", "Delta"),
            List("Total", "Brent", "Brent", "WTI",   "WTI",   "WTI"),
            List("Total", "Total", "Jan",   "Total", "Feb",   "Jan") ),
      List( List(7+1+4,    4,        4,       7+1,      7,       1) )
    )
  }

  @Test
  def testRowSubTotals {
    check(
      pivotTableDataSource,
      PivotFieldsState(dataFields=List(Field("Delta")), rowFields=List(Field("Market"), Field("Period"))),
      Totals(false, true, false, false),
      List( List("Total", "Total"),
            List("Brent", "Total"),
            List("Brent", "Jan"),
            List("WTI", "Total"),
            List("WTI", "Feb"),
            List("WTI", "Jan") ),
      List( List(""), List("Delta") ),
      List( List(7+1+4),
            List(4),
            List(4),
            List(7+1),
            List(7),
            List(1))
    )
  }

  @Test
  def testRowSubTotalsWithOneDisabled {
    check(
      pivotTableDataSource,
      PivotFieldsState(dataFields=List(Field("Delta")), rowFields=List(Field("Market"), Field("Period"))),
      Totals(false, true, false, false),
      List( List("Total", "Total"),
            List("Brent", "Jan"),
            List("WTI", "Feb"),
            List("WTI", "Jan") ),
      List( List(""), List("Delta") ),
      List( List(7+1+4),
            List(4),
            List(7),
            List(1)),
      rowDisabledSubTotals = List(Field("Market"))
    )
  }

  @Test
  def testMixedRowAndColumnFieldsWithSubTotals{
    check(
      pivotTableDataSource,
      PivotFieldsState(dataFields=List(Field("Delta")), columnFields=List(Field("Market")), rowFields=List(Field("Period"))),
      Totals(false, true, false, true),
      List( List("Total"), List("Feb"), List("Jan") ),
      List( List("Total", "Brent", "WTI"), List("Delta", "Delta", "Delta") ),
      List( List(7+1+4, 4, 7+1),
            List(7, "", 7),
            List(1+4, 4, 1)
      )
    )
  }

  @Test
  def testTwoDataFieldsWithDifferentCombiningBehaviour {
    check(
      pivotTableDataSource,
      new PivotFieldsState(columns=ColumnTrees(List(
        ColumnTree.dataField(Field("Delta")),
        ColumnTree.dataField(Field("Period"))
      ))),
      Totals.Null,
      List( List("") ),
      List( List("",""), List("Delta", "Period") ),
      List( List( 4+1+7, "2 values" ) )
    )
  }

  @Test
  def testDataFieldsOrderIsPreserved {
    check(
      pivotTableDataSource,
      new PivotFieldsState(columns=ColumnTrees(List(
        ColumnTree.dataField(Field("Day Change")),
        ColumnTree.dataField(Field("Delta"))
      ))),
      Totals.Null,
      List( List("") ),
      List( List("",""), List("Day Change", "Delta") ),
      List( List( 118, 12 ) )
    )
  }

  @Test
  def testNoDataField{
    check(
      pivotTableDataSource,
      PivotFieldsState(columnFields=List(Field("Market"))),
      Totals.Null,
      List( List("") ),
      List( List("","",""), List("Brent", "Unused", "WTI") ), //Unused is show because we have not specified a data field
      List( List("", "", "") )
    )
  }

  @Test
  def testEmpty {
    check(
      pivotTableDataSource,
      PivotFieldsState(),
      Totals.Null,
      List( List("") ),
      List( List(""), List("") ),
      List( List("") )
    )
  }

  @Test
  def testOnlyRows {
    check(
      pivotTableDataSource,
      PivotFieldsState(rowFields=List( Field("Market"))),
      Totals.Null,
      List( List("Brent"), List("Unused"), List("WTI") ), //Unused is shown because there is no data field
      List( List(""), List("") ),
      List( List(""), List(""), List(""))
    )
  }

  @Test
  def testTwoRowFields {
    check(
      pivotTableDataSource,
      PivotFieldsState(rowFields=List( Field("Market"), Field("Period") )),
      Totals.Null,
      List( List("Brent", "Jan"), List("Unused", "n/a"), List("WTI", "Feb"), List("WTI", "Jan") ),
      List( List(""), List("") ),
      List( List(""), List(""), List(""), List(""))
    )
  }

  @Test
  def testColumnTreeWithDifferentDepths {
    check(
      pivotTableDataSource,
      new PivotFieldsState(columns=ColumnTrees(List(
        ColumnTree(Field("Market"), false,
          ColumnTree.dataField(Field("Delta")),
          ColumnTree(Field("Day Change Component"), false, ColumnTree.dataField(Field("Day Change"))))
        )
      )),
      Totals.Null,
      List( List("") ),
      List( List("Brent", "Brent", "Brent", "WTI"),
            List("Delta", "Price", "Vol",   "Delta"),
            List("", "Day Change",   "Day Change",   "") ),
      List( List(4, 100, 18, 8) )
    )
  }

  @Test
  def testToFlatRows() {
    val fieldsState = new PivotFieldsState(
      columns=ColumnTrees(List(
        ColumnTree(Field("Market"), false,
          ColumnTree.dataField(Field("Delta")),
          ColumnTree(Field("Day Change Component"), false, ColumnTree.dataField(Field("Day Change")))
        ))
      ),
      rowFields = List(Field("Period"))
    )
    val pivotTable = PivotTableModel.createPivotTableData(pivotTableDataSource, Some(fieldsState))
    val flatRows = pivotTable.toFlatRows(Totals.Null)
    assertEquals(
      flatRows.map(_.map(_.toString)),
      List(
        List("",       "Brent", "Brent",      "Brent",      "WTI"  ),
        List("",       "Delta", "Price",      "Vol",        "Delta"),
        List("Period", "",      "Day Change", "Day Change", ""     ),
        List("Feb",    "",      "",           "",           "7"    ),
        List("Jan",    "4",     "100",        "18",         "1"    )
      )
    )
  }

  def check(
          pivot:PivotTableDataSource,
          fieldState:PivotFieldsState,
          totals:Totals,
          expectedRows:List[List[String]],
          expectedColumns:List[List[String]],
          expectedData:List[List[Any]],
          rowDisabledSubTotals:List[Field]=List()
  ) {
    val pivotTable = PivotTableModel.createPivotTableData(pivot, Some(fieldState))
    val converter = new PivotTableConverter(OtherLayoutInfo(totals = totals, rowSubTotalsDisabled = rowDisabledSubTotals), pivotTable)
    val (rowData, colData, mainData) = converter.allTableCells()
    val rows = rowData.map(_.map(_.label).toList).toList
    val columns = colData.map(_.map(_.label).toList).toList
    val data = mainData.map(_.map(_.asString).toList).toList
    val expectedDataToString = expectedData.map(_.map(_.toString))
    assertEquals(rows, expectedRows)
    assertEquals(columns, expectedColumns)
    assertEquals(data, expectedDataToString)
  }
}
