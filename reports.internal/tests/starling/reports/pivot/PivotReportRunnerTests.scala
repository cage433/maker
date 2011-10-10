package starling.reports.pivot

import org.scalatest.matchers.ShouldMatchers
import org.mockito.Mockito._
import org.mockito.Matchers._
import collection.immutable.Nil
import org.testng.annotations.{BeforeTest, Test}
import starling.utils.StarlingMatchers._
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import java.io.{PrintWriter, StringWriter}
import starling.utils.StarlingTest
import starling.gui.api.{ReportParameters, UserReportData, UserReport}
import starling.rmi.{PivotData, StarlingServer}
import starling.daterange.Day
import starling.pivot.{PivotFieldParams, PivotLayout, PivotFieldsState}
import org.mockito.Matchers
import starling.pivot.controller.PivotTable


class PivotReportRunnerTests extends StarlingTest with ShouldMatchers {
  //lazy val pivotReportRunner = new ReportService()


}