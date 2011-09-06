package starling.http

import org.scalatest.matchers.ShouldMatchers
import org.mockito.Mockito._
import org.mockito.Matchers._
import collection.immutable.Nil
import org.testng.annotations.{BeforeTest, Test}
import starling.utils.StarlingMatchers._
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import java.io.{PrintWriter, StringWriter}
import starling.gui.api.{ReportParameters, UserReportData, UserReport}
import starling.daterange.Day
import starling.pivot.{PivotFieldParams, PivotLayout, PivotFieldsState}
import org.mockito.Matchers
import starling.pivot.controller.PivotTable
import starling.reports.pivot.ReportService
import starling.rmi.{UserReportsService, PivotData, StarlingServer}
import starling.auth.User
import starling.services.utils.RegressionRunner
import starling.utils.{StarlingTest}

class ReportServletTest extends StarlingTest with ShouldMatchers {
  lazy val reportServlet = new ReportServlet("reports", userReportsService)

  val httpRequest = mock(classOf[HttpServletRequest])
  val httpResponse = mock(classOf[HttpServletResponse])
  val userReportsService = mock(classOf[UserReportsService])

  @Test
  def shouldListReportsWhenRequestingSlash {

    val writer = new StringWriter()
    val defaultDayText = RegressionRunner.defaultDay.toString
    when(httpRequest.getRequestURI) thenReturn "/reports"
    when(httpResponse.getWriter) thenReturn new PrintWriter(writer)
    when(userReportsService.allUserReports) thenReturn Map("XuserX" -> List(new UserReport("Xreport1X", null, true), new UserReport("Xreport2X", null, true)))
    reportServlet.doGet(httpRequest, httpResponse)

    val expected =
      "<a href=\"/reports/XuserX/Xreport1X/"+defaultDayText+"\">XuserX Xreport1X " + defaultDayText + "</a> <br>\n" +
      "<a href=\"/reports/XuserX/Xreport2X/"+defaultDayText+"\">XuserX Xreport2X " + defaultDayText + "</a> <br>\n"
    writer.toString should equal (expected)
  }

  @Test
  def shouldListReportsAsTxtWhenRequestingList {

    val writer = new StringWriter()
    val defaultDayText = RegressionRunner.defaultDay.toString
    when(httpRequest.getRequestURI) thenReturn "/reports/list"
    when(httpResponse.getWriter) thenReturn new PrintWriter(writer)
    when(userReportsService.allUserReports) thenReturn Map("XuserX" -> List(new UserReport("Xreport1X", null, true), new UserReport("Xreport2X", null, true)))
    reportServlet.doGet(httpRequest, httpResponse)

    val expected =
      "XuserX/Xreport1X\n" +
      "XuserX/Xreport2X\n"
    writer.toString should equal (expected)
  }

  @Test
  def shouldRenderReportWhenRequestingReport {

    val layout = mock(classOf[PivotFieldsState])
    val pivotTable = mock(classOf[PivotTable])
    val day = Day(2010, 1, 1)

    val writer = new StringWriter()
    when(httpRequest.getRequestURI) thenReturn "/reports/XuserX/Xreport1X/01Jan2010"
    when(httpResponse.getWriter) thenReturn new PrintWriter(writer)
    when(userReportsService.runNamedReport(User("XuserX"), "Xreport1X", day, None)) thenReturn Some(pivotTable)

    when(pivotTable.asCSV) thenReturn "<ascsv>"

    reportServlet.doGet(httpRequest, httpResponse)

    writer.toString should equal ("<ascsv>")

  }

}