package starling.services.excel

import java.lang.String
import org.boris.xlloop.reflect.XLFunction
import starling.daterange._
import starling.rmi.UserReportsService
import starling.auth.User
import starling.pivot._
import starling.loopyxl.ExcelMethod
import collection.immutable.List

class ReportHandler(userReportsService : UserReportsService) {

  @ExcelMethod
  @XLFunction(
    name = "runReport",
    category = "Starling",
    args = Array("ReportName", "identifier"),
    argHelp = Array(
      "Report Name",
      "Observation day",
      "Layout Name"))
  def runReport(
                 reportName: String,
                 observationDate: Double,
                 optionalLayoutName: String): Array[Array[Any]] = {
    val layout = if (optionalLayoutName == null || optionalLayoutName.trim.isEmpty) None else Some(optionalLayoutName)
    val day = Day.fromExcel(observationDate)
    val userName = User.currentlyLoggedOn.username
    val pivotTable = userReportsService.runNamedReport(User(userName), reportName, day, layout).get

    val rows = pivotTable.toFlatRows(Totals.Null)
    rows.map(_.toArray).toArray
  }

  @ExcelMethod
  @XLFunction(name="position",category = "Starling")
  def position(reportName: String, observationDate: Double, market: String, period: String): Any = reportValue("Position",
    Map("Report Name" -> reportName, "Day" -> observationDate), List("Risk Market", market, "Risk Period", period) : _*)

  @ExcelMethod
  def reportValue(measure: String, dataSourceParameters: Map[String, Any], filters : AnyRef*): Any = {
    val reportName = dataSourceParameters("Report Name").asInstanceOf[String]
    val observationDay = Day.fromExcel(dataSourceParameters("Day").asInstanceOf[Double])

    val filtersAsField = filters.grouped(2).map(_.toList).map{case List(field: String, value) => (Field(field), value)}.toMap

    val pivotTable = userReportsService.pivotTableFor(User.currentlyLoggedOn, reportName, observationDay,
      PivotFieldsState(rowFields = filtersAsField.keys.toList, dataFields = List(Field(measure))))

    val tableCell = pivotTable.cell(measure, filtersAsField.toSeq : _*)

    tableCell.asInstanceOf[PivotQuantity].doubleValue.getOrElse(tableCell.toString)
  }
}
