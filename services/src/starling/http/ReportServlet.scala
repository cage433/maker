package starling.http

import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import starling.daterange.Day
import starling.pivot.PivotFieldParams
import starling.reports.pivot.ReportService
import starling.utils.ImplicitConversions._
import starling.rmi.{UserReportsService, StarlingServer}
import starling.auth.User
import starling.utils.Pattern.Extractor
import starling.pivot.controller.PivotTable
import starling.utils.{Utils, RegressionRunner}
import starling.utils.ImplicitConversions._
import scalaz._
import Scalaz._

/**
 * Exposes saved reports as csv files
 */

class ReportServlet(prefix:String, userReportsService:UserReportsService) extends HttpServlet {

  override def doGet(request: HttpServletRequest, response: HttpServletResponse) = {
    var url = request.getRequestURI
    val path = url.substring(prefix.length + 1)

    if (path == "" || path == "/") {
      response.setContentType("text/html")
      val writer = response.getWriter
      userReportsService.allUserReports.foreach { case(user,reports) => {
        reports.foreach { report=> {
          writer.write( <a href={"/reports/" + user.urlEncode + "/" + report.reportName.urlEncode + "/" + RegressionRunner.defaultDay}>{user} {report.reportName} {RegressionRunner.defaultDay.toString}</a>.toString )
          writer.write( " <br>\n" )
        } }
      } }
    } else if (path == "/list") {
      response.setContentType("text/plain")
      val writer = response.getWriter
      userReportsService.allUserReports.foreach { case(user,reports) => {
        reports.foreach { report=> {
          writer.write( user + "/" + report.reportName + "\n" )
        } }
      } }
    } else {
      val split = path.substring(1).split("/")
      val userName = split(0).urlDecode
      val reportName = split(1).urlDecode
      val day = Day.parse(split(2))

      val (contentType, content) = userReportsService.runNamedReport(User(userName), reportName, day, None) match {
        case Some(pivotData) => request.getHeader("Accept") ?? "text/plain" match {
          case accept if accept.containsOneOf("text/html", "*/*") => "text/html" → pivotData.convertUsing(Utils.tableConverter)
          case accept => "text/plain" → pivotData.asCSV
        }
        case None => throw new Exception("No report found named " + reportName + " " + userName)
      }

      response.setContentType(contentType)
      response.getWriter.write(content)
    }
  }
}