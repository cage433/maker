package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.utils.StarlingXStream
import starling.gui.api.UserReport._
import starling.gui.api.{UserReport, UserReportData}
import starling.pivot.PivotFieldParams._
import starling.pivot._
import system.{PatchUtils, Patch}

class Patch103_ChangeReportsToBookmarks extends Patch {
  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch103_CreateBookmarksTableAgain.sql"))

    val layouts = starling.queryWithResult("SELECT * from PivotLayouts") {
      rs => {
        val associatedReports = {
          val ar = rs.getString("associatedReport")
          if (ar != null) {
            ar.split(PivotLayout.AssociatedReportsDelimiter).toList
          } else {
            List()
          }
        }
        val username = rs.getString("starlingUser")
        (username,
        PivotLayout(rs.getString("layoutName"),
          StarlingXStream.read(rs.getString("layout")).asInstanceOf[PivotFieldsState],
          true, StarlingXStream.read(rs.getString("otherLayoutInfo")).asInstanceOf[OtherLayoutInfo],
          rs.getString("layoutType"), associatedReports))
      }
    }
    val layoutsWithAssociatedReports = layouts.filter{ case (_,l) => l.associatedReports.nonEmpty}
    layoutsWithAssociatedReports.foreach{case (username, layout) => {
      layout.associatedReports.foreach(report => {

        val nameAndReports = starling.queryWithResult("select * from UserReports where starlingUser = :user and reportName = :reportName",
          Map("user" -> username, "reportName" -> report)) {
          rs => {
            val report = UserReport(rs.getString("reportName"), StarlingXStream.read(rs.getString("report")).asInstanceOf[UserReportData],
              rs.getBoolean("showParameters"))
            val name = report.reportName + "-" + layout.layoutName
            (name, report)
          }
        }

        val pfs = Some(layout.pivotFieldState)

        val bookmarks = nameAndReports.map{case (name, report0) => {
          val pps = FakePivotPageState(false, PivotFieldParams(true, pfs), layout.otherLayoutInfo)
          (name, FakeReportBookmark(report0.showParameters, report0.data, pps))
        }}

        val bookmarksText = bookmarks.map{case (name, bookmark) => (name, StarlingXStream.write(bookmark))}

        val updatedBookmarksText = bookmarksText.map{case (name,t) => {
          (name, t.replaceAll("starling.schemaevolution.FakeReportBookmark", "starling.gui.ReportBookmark"))
        }}

        updatedBookmarksText.foreach{case (name0, bookmark) => {
          val name = name0.take(300)
          writer.insert("Bookmarks", Map("starlingUser" -> username, "bookmarkName" -> name, "bookmark" -> bookmark))
        }}

      })
    }}
  }
}

case class FakePivotPageState(showChart:Boolean=false, pivotFieldParams:PivotFieldParams=PivotFieldParams(true, None),
                          otherLayoutInfo:OtherLayoutInfo = OtherLayoutInfo(totals = Totals.Null))

case class FakeReportBookmark(showParameters:Boolean, userReportData:UserReportData, pivotPageState:FakePivotPageState)