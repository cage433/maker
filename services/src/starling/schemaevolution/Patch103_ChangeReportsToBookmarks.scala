package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.instrument.utils.StarlingXStream
import starling.gui.api.UserReport._
import starling.gui.api.{UserReport, UserReportData}
import starling.pivot.PivotFieldParams._
import starling.pivot._
import model.CollapsedState
import system.{PatchUtils, Patch}
import xstream.{Fields, Reader, MapBasedConverter}
import starling.pivot.HiddenType._

class Patch103_ChangeReportsToBookmarks extends Patch {
  val convertingXStream = StarlingXStream.createXStream
  convertingXStream.registerConverter(new MapBasedConverter(
    StarlingXStream.createXStream,
    classOf[OtherLayoutInfo],
    new Reader {
      def create(fields:Fields) = {
        val totals = fields.getFieldValue("totals").getOrElse(Totals.Null).asInstanceOf[Totals]
        val frozen = fields.getFieldValue("frozen").getOrElse(true).asInstanceOf[Boolean]
        val fieldPanelCollapsed = fields.getFieldValue("fieldPanelCollapsed").getOrElse(false).asInstanceOf[Boolean]
        val rowCollapsedState = fields.getFieldValue("rowCollapsedState").getOrElse(CollapsedState.None).asInstanceOf[CollapsedState]
        val columnCollapsedState = fields.getFieldValue("columnCollapsedState").getOrElse(CollapsedState.None).asInstanceOf[CollapsedState]

        val rowSubTotalsDisabled = fields.getFieldValue("rowSubTotalsDisabled").getOrElse(List()).asInstanceOf[List[Field]]
        val columnSubTotalsDisabled = fields.getFieldValue("columnSubTotalsDisabled").getOrElse(List()).asInstanceOf[List[Field]]
        val newDisabledSubTotals = (rowSubTotalsDisabled ::: columnSubTotalsDisabled).toSet.toList

        val hiddenType = if (fieldPanelCollapsed) FieldListHidden else NothingHidden

        OtherLayoutInfo(totals, frozen, rowCollapsedState, columnCollapsedState, newDisabledSubTotals, hiddenType = hiddenType)
      }
    },
    Map("fieldPanelCollapsed" -> classOf[Boolean])
  ))


  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch103_CreateBookmarksTableAgain.sql"))

    val text = """<rowCollapsedState><elements/></rowCollapsedState><columnCollapsedState reference="../rowCollapsedState"/>"""
    val textToUse = """<rowCollapsedState><elements/></rowCollapsedState><columnCollapsedState reference="../rowCollapsedState"/><disabledSubTotals reference="../rowCollapsedState/elements"/></starling.pivot.OtherLayoutInfo>"""
    writer.queryForUpdate("SELECT otherLayoutInfo from PivotLayouts") {
      rs => {
        val oldText = rs.getString("otherLayoutInfo")
        if (!oldText.contains(text)) {
          val startIndex = oldText.indexOf("""<rowCollapsedState>""")
          val endText = """</columnCollapsedState>"""
          val endIndex = oldText.indexOf(endText) + endText.length()
          if (startIndex != -1 && endIndex != -1) {
            val beginText = oldText.take(startIndex)
            val newText = beginText + textToUse
            rs.update(Map("otherLayoutInfo" -> newText))
          }
        }
      }
    }

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
        val layoutName = rs.getString("layoutName")
        (username,
        PivotLayout(layoutName,
          StarlingXStream.read(rs.getString("layout")).asInstanceOf[PivotFieldsState],
          true, convertingXStream.fromXML(rs.getString("otherLayoutInfo")).asInstanceOf[OtherLayoutInfo],
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