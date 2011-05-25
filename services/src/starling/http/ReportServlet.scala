package starling.http

import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import starling.daterange.Day
import starling.pivot.PivotFieldParams
import starling.reports.pivot.ReportService
import starling.utils.ImplicitConversions._
import starling.rmi.{UserReportsService, StarlingServer}
import starling.auth.User
import starling.utils.RegressionRunner

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
      response.setContentType("text/plain")
      val split = path.substring(1).split("/")
      val userName = split(0).urlDecode
      val reportName = split(1).urlDecode
      val day = Day.parse(split(2))
      userReportsService.runNamedReport(User(userName), reportName, day, None) match {
        case Some(pivotData) => {
          val writer = response.getWriter
          val csv = pivotData.asCSV
          writer.write(csv)
        }
        case None => throw new Exception("No report found named " + reportName + " " + userName)
      }
    }
  }
}