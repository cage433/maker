package starling.reports.internal

import starling.auth.User
import org.mockito.Mockito._
import collection.immutable.Nil
import starling.utils.StarlingTest
import org.testng.annotations.{AfterClass, BeforeClass, Test}
import org.testng.Assert._
import starling.pivot.controller.PivotTable
import starling.pivot.model.{ValueAxisValueType, AxisValue}
import starling.daterange.Day
import starling.pivot._
import starling.quantity.UOM
import org.scalatest.matchers.ShouldMatchers

class ReportHandlerTests extends StarlingTest with ShouldMatchers {
  lazy val reportHandler = new ReportHandler(userReportService)

  // Collaborators
  var userReportService: UserReportsService = _

  // Data
  val user = User("<userName>", "<name>", None, Nil, "<phoneNumber>", "<email>", "<departments>")
  val reportName = "<reportName>"
  val day = Day(2010, 1, 1)
  val pivotTable = mock(classOf[PivotTable])
  val excelDay = day.toExcel

  @BeforeClass
  def initialise {
    User.setLoggedOn(Some(user))

    userReportService = mock(classOf[UserReportsService])
  }

  @AfterClass
  def tearDown {
    User.setLoggedOn(None)
  }

  @Test
  def shouldExtractPositionForRiskMarketAndPeriod() {
    val tableCell = TableCell.fromPivotQuantity(new PivotQuantity(1000, UOM.BBL), PivotFormatter.DefaultExtraFormatInfo)
    val fieldState = PivotFieldsState(rowFields = List(Field("Risk Market"), Field("Risk Period")), dataFields = List(Field("Position")))
    def av(field:String, value:Any) = AxisValue(Field(field), ValueAxisValueType(value), 1)
    val bucketData = Map((List( av("Risk Market", "<WTI>"), av("Risk Period", "<Jan2010>")), List[AxisValue]()) -> tableCell)

    fieldState.equals(fieldState) should be === true

    when(userReportService.pivotTableFor(user, reportName, day, fieldState)) thenReturn pivotTable
    when(pivotTable.cell("Position", (Field("Risk Market"), "<WTI>"), (Field("Risk Period"), "<Jan2010>")).asInstanceOf[PivotQuantity]) thenReturn new PivotQuantity(1000, UOM.BBL)

    assert(reportHandler.position(reportName, excelDay, "<WTI>", "<Jan2010>") == 1000.0)
  }

  @Test
  def shouldRunReportAndFormatAsArrayMatrix {
    val result = List(List("a", "b"))
    when(userReportService.runNamedReport(user, reportName, day, None)) thenReturn Some(pivotTable)
    when(pivotTable.toFlatRows(Totals.Null)) thenReturn result

    assert(reportHandler.runReport(reportName, excelDay, "").map(_.toList).toList == result)
    verify(userReportService).runNamedReport(user, reportName, day, None)
  }
}
